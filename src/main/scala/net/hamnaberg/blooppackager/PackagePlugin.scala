package net.hamnaberg.blooppackager

import bleep.internal.FileUtils
import bleep.internal.compat.{OptionalCompatOps, ListCompatOps}
import bleep.logging.Logger
import bloop.config.Config

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.FileTime
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.zip.ZipEntry
import scala.math.Ordered.orderingToOrdered
import scala.util.Using

object PackagePlugin {
  private val epochTime = FileTime.fromMillis(0)

  def run(logger: Logger, projects: List[Config.Project], cmd: PackageCommand): Either[String, Unit] = {
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
          jar(logger, project) match {
            case Some(jar) => logger.withContext(jar).info("built jar file")
            case None => ()
          }
        case PackageCommand.Dist(_, programs, distPath) =>
          val distDir = distPath.map(_.resolve(project.name)).getOrElse(project.out.resolve("dist"))
          Files.createDirectories(distDir)
          val lib = distDir.resolve("lib")
          FileUtils.deleteDirectory(lib)
          Files.createDirectories(lib)

          val jarFiles = dependenciesFor(logger, project, dependencyLookup).distinct

          jarFiles.foreach { src =>
            Files.copy(src, lib.resolve(src.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
          }
          if (programs.nonEmpty) {
            val bin = distDir.resolve("bin")
            FileUtils.deleteDirectory(bin)
            Files.createDirectories(bin)
            Scripts.writeScripts(bin, "", programs)
          }

          logger.withContext(distDir).info("dist complete")
      }
    }
    Right(())
  }

  def dependenciesFor(logger: Logger, project: Config.Project, lookup: Map[Path, (Config.Project)]): List[Path] = {
    val (dirs, jars) = project.classpath.partition(Files.isDirectory(_))
    val mayBeJar = jar(logger, project)
    mayBeJar.toList ::: jars ::: dirs
      .flatMap(dir => lookup.get(dir).toList)
      .flatMap(dependantProject => dependenciesFor(logger, dependantProject, lookup))
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

  def jar(logger: Logger, project: Config.Project): Option[Path] = {
    val jarFile = project.out.resolve(s"${project.name}-jvm.jar")
    val internal = project.out.resolve("bloop-internal-classes")

    val resourceLastChange: Option[FileTime] = project.resources
      .getOrElse(Nil)
      .filter(Files.exists(_))
      .flatMap(p =>
        Files
          .walk(p)
          .filter(Files.isRegularFile(_))
          .map[FileTime](file => Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS))
          .max(_.compareTo(_))
          .toScalaCompat
      )
      .maxOptionCompat

    if (Files.exists(internal)) {
      val previousPath = project.out.resolve(".previous-classes-directory")

      val previous: Option[Path] =
        if (Files.exists(previousPath)) Some(Paths.get(Files.readString(previousPath))) else None

      val classesDir = Files
        .list(internal)
        .filter(_.getFileName.toString.startsWith("classes-bloop-cli"))
        .findFirst()
        .toScalaCompat

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
          buildJar(logger, project, jarFile, classes)
        } else if (Files.exists(jarFile) && resourceLastChange.exists(change => change > Files.getLastModifiedTime(jarFile))) {
          buildJar(logger, project, jarFile, classes)
        }
      }
    }
    if (Files.exists(jarFile)) Some(jarFile) else None
  }

  private def buildJar(logger: Logger, project: Config.Project, file: Path, classes: Path): Unit = {
    val resourceDirectories = project.resources.getOrElse(Nil)
    if (Files.deleteIfExists(file)) {
      logger.debug(s"Deleted existing $file")
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
