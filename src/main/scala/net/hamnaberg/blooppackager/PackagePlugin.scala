package net.hamnaberg.blooppackager

import bloop.config.Config

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.zip.ZipEntry
import scala.jdk.OptionConverters._
import scala.math.Ordered.orderingToOrdered
import scala.util.Using

object PackagePlugin {
  private val epochTime = FileTime.fromMillis(0)

  def run(projects: List[Config.Project], cmd: PackageCommand): Either[String, Unit] = {
    val filtered: List[Config.Project] = cmd match {
      case PackageCommand.Jars(Nil) =>
        projects
      case PackageCommand.Jars(requestedProjects) =>
        projects.collect { case p if requestedProjects.contains(p.name) => p }
      case PackageCommand.Dist(project, _, _) =>
        projects.collect { case p if p.name == project => p }
    }

    val dependencyLookup = projects.map(p => p.classesDir -> p).toMap

    filtered.foreach { project =>
      cmd match {
        case PackageCommand.Jars(_) =>
          val maybeJar = jar(project)
          maybeJar.foreach(println)
        case PackageCommand.Dist(_, programs, distPath) =>
          val distDir = distPath.map(_.resolve(project.name)).getOrElse(project.out.resolve("dist"))
          Files.createDirectories(distDir)
          val lib = distDir.resolve("lib")
          deleteDirectory(lib)
          Files.createDirectories(lib)

          val jarFiles = dependenciesFor(project, dependencyLookup).distinct

          jarFiles.foreach { src =>
            Files.copy(src, lib.resolve(src.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
          }
          if (programs.nonEmpty) {
            val bin = distDir.resolve("bin")
            deleteDirectory(bin)
            Files.createDirectories(bin)
            Scripts.writeScripts(bin, "", programs)
          }

          println(distDir)
      }
    }
    Right(())
  }

  private def deleteDirectory(dir: Path): Unit =
    if (Files.exists(dir)) {
      Files.walkFileTree(
        dir,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
    }

  def dependenciesFor(project: Config.Project, lookup: Map[Path, (Config.Project)]): List[Path] = {
    val (dirs, jars) = project.classpath.partition(Files.isDirectory(_))
    val mayBeJar = jar(project)
    mayBeJar.toList ::: jars ::: dirs
      .flatMap(dir => lookup.get(dir).toList)
      .flatMap(dependantProject => dependenciesFor(dependantProject, lookup))
  }

  def buildManifest(project: Config.Project): Manifest = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.IMPLEMENTATION_TITLE, project.name)
    val jvmMainClass = project.platform.collect { case x: Config.Platform.Jvm => x }.flatMap(_.mainClass)
    jvmMainClass.foreach { cls =>
      manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, cls)
    }
    manifest
  }

  def jar(project: Config.Project): Option[Path] = {
    val jarFile = project.out.resolve(s"${project.name}-jvm.jar")
    val internal = project.out.resolve("bloop-internal-classes")

    val resourceLastChange: Option[FileTime] = project.resources
      .getOrElse(Nil)
      .filter(Files.exists(_))
      .flatMap(p =>
        Files
          .walk(p)
          .filter(Files.isRegularFile(_))
          .map(file => Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS))
          .max(_.compareTo(_))
          .toScala
      )
      .maxOption

    if (Files.exists(internal)) {
      val previousPath = project.out.resolve(".previous-classes-directory")

      val previous = Option.when(Files.exists(previousPath))(Files.readString(previousPath)).map(Paths.get(_))
      val classesDir = Files
        .list(internal)
        .filter(_.getFileName.toString.startsWith("classes-bloop-cli"))
        .findFirst()
        .toScala

      classesDir.foreach { classes =>
        val nonEmptyDir = Files.list(classes).findFirst().isPresent //detect if we are empty

        if (!previous.contains(classes) && nonEmptyDir) {
          Files.writeString(
            previousPath,
            classes.toString,
            StandardCharsets.UTF_8,
            StandardOpenOption.WRITE,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING
          )
          buildJar(project, jarFile, classes)
        } else if (Files.exists(jarFile) && resourceLastChange.exists(change => change > Files.getLastModifiedTime(jarFile))) {
          buildJar(project, jarFile, classes)
        }
      }
    }
    Option.when(Files.exists(jarFile))(jarFile)
  }

  private def buildJar(project: Config.Project, file: Path, classes: Path): Unit = {
    val resourceDirectories = project.resources.getOrElse(Nil)
    if (Files.deleteIfExists(file)) {
      Console.err.println(s"Deleted existing $file")
    }
    if (Files.notExists(file)) {
      val manifest = buildManifest(project)
      Using.resource(new JarOutputStream(Files.newOutputStream(file, StandardOpenOption.CREATE_NEW), manifest)) { os =>
        addFilesToJar(classes, os)
        resourceDirectories.filter(Files.exists(_)).foreach { resourceDir =>
          addFilesToJar(resourceDir, os)
        }
      }
    }
  }

  private def addFilesToJar(root: Path, os: JarOutputStream): Unit =
    Files.walk(root).forEachOrdered { file =>
      val name = root.relativize(file).toString
      if (name.nonEmpty) {
        addJarEntry(os, file, name, Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS))
      }
    }

  private def addJarEntry(os: JarOutputStream, file: Path, name: String, directory: Boolean): Unit = {
    val entry = new ZipEntry(if (directory) s"$name/" else name)
    os.putNextEntry(entry)

    entry.setCreationTime(epochTime)
    entry.setLastModifiedTime(epochTime)
    entry.setLastAccessTime(epochTime)
    if (!directory) {
      entry.setMethod(ZipEntry.DEFLATED)
      entry.setSize(Files.size(file))
      Using.resource(Files.newInputStream(file))(is => is.transferTo(os))
    }
    ()
  }
}
