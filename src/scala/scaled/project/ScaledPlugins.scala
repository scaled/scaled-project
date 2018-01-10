//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman._

@Plugin(tag="project-root")
class ScaledRootPlugin extends RootPlugin {
  def checkRoot (root :Path) :Int = if (Scaled.isScaledRoot(root)) 1 else -1
}

@Plugin(tag="project-resolver")
class ScaledResolverPlugin extends ResolverPlugin {
  import Scaled._

  def findPackage (root :Path, cur :Path) :Path = {
    if (cur == null) throw new IllegalArgumentException("Unable to find 'package.scaled' in root.")
    val pfile = cur.resolve("package.scaled");
    if (Files.exists(pfile)) pfile else findPackage(root, cur.getParent)
  }

  override def metaFiles (root :Project.Root) =
    Seq(root.path.resolve("module.scaled"), findPackage(root.path, root.path))

  override def addComponents (project :Project) {
    val rootPath = project.root.path
    val pkgFile = findPackage(rootPath, rootPath)
    val modFile = rootPath.resolve("module.scaled") // may not exist

    val pkg = new Package(pkgFile) ; val modName = rootPath.getFileName.toString
    val cfg = new pacman.Config(Seq.empty[String].asJList)
    val mod = if (pkgFile.getParent != rootPath) Option(pkg.module(modName)) getOrElse {
      project.log(s"Error: $pkgFile contains no module declaration for $modFile")
      new Module(pkg, modName, modFile.getParent, pkg.source, cfg) // fake it!
    }
    // if we're in the top-level of a multi-module package, we'll have no module; in that case we
    // fake up a default module to avoid a bunch of special casery below
    else Option(pkg.module(Module.DEFAULT)) getOrElse {
      new Module(pkg, Module.DEFAULT, rootPath, pkg.source, cfg)
    }

    val targetDir = rootPath.resolve("target")
    val classesDir = targetDir.resolve("classes")
    val resourceDir = rootPath.resolve("src/resources")

    // add a filer component with custom ignores
    val igns = Ignorer.stockIgnores
    igns += Ignorer.ignorePath(targetDir, rootPath)
    project.addComponent(classOf[Filer], new DirectoryFiler(project, igns))

    // add a sources component with our source directories
    val sources = new Sources(Seq(rootPath.resolve("src")))
    project.addComponent(classOf[Sources], sources)

    // add our dependencies
    val depends = new ScaledDepends(project, pkg, mod)
    project.addComponent(classOf[Depends], depends)

    // add a java component
    val javaComp = new JavaComponent(project)
    project.addComponent(classOf[JavaComponent], javaComp)

    // init our JavaComponent
    val classpath = classesDir +: depends.moddeps.dependClasspath.toSeqV
    javaComp.javaMetaV() = new JavaMeta(Seq(classesDir), classesDir, classpath, classpath)
    javaComp.addTesters()

    // TODO: this is expensive, can we do something cheaper
    val ssum = sources.summarize
    // TODO: do we want to try to support multi-lingual projects? that sounds like a giant PITA,
    // but we could probably at least generate a warning if we have some crazy mishmash of sources
    // TEMP: if we have any Kotlin files, we just use the KotlinCompiler
    if (ssum.contains("kt")) {
      project.addComponent(classOf[Compiler], new KotlinCompiler(project, javaComp) {
        override def javacOpts = pkg.jcopts.toSeq
        // override def kotlincOpts = pkg.kcopts.toSeq
        override def kotlincVers = depends.artifactVers(
          "org.jetbrains.kotlin", "kotlin-stdlib", super.kotlincVers)
        override protected def willCompile () {
          if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, javaComp.outputDir)
        }
      })

    } else {
      project.addComponent(classOf[Compiler], new ScalaCompiler(project, javaComp) {
        override def javacOpts = pkg.jcopts.toSeq
        override def scalacOpts = pkg.scopts.toSeq
        override def scalacVers = depends.artifactVers(
          "org.scala-lang", "scala-library", super.scalacVers)
        override protected def willCompile () {
          if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, javaComp.outputDir)
        }
      })
    }

    val name = if (mod.isDefault) pkg.name else s"${pkg.name}-${mod.name}"
    val testRoot = if (mod.name == "test") None
                   else pkg.modules.find(_.name == "test").map(m => Project.Root(m.root))
    project.metaV() = Project.Meta(name, Set(toSrcURL(mod.source)), testRoot)
  }
}
