//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import codex.extract.SourceSet
import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman._
import scaled.util.{BufferBuilder, Close}

class ScaledProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  import ScaledProject._

  private def rootPath = root.path
  private[this] val pkgFile = findPackage(rootPath, rootPath)
  private[this] val modFile = rootPath.resolve("module.scaled") // may not exist
  private[this] val modV = Value[Module](null)

  private val javaComp = new JavaComponent(this)
  addComponent(classOf[JavaComponent], javaComp)

  def mod :Module = modV()
  def pkg :Package = mod.pkg

  // reinit if package.scaled (or module.scaled) changes
  { val watchSvc = metaSvc.service[WatchService]
    watchSvc.watchFile(pkgFile, file => init())
    if (Files.exists(modFile)) watchSvc.watchFile(modFile, file => init())
  }
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  // TODO: do all this resolution on a background thread?
  override def init () {
    val pkg = new Package(pkgFile) ; val modName = rootPath.getFileName.toString
    val cfg = new pacman.Config(Seq.empty[String].asJList)
    val mod = if (pkgFile.getParent != rootPath) Option(pkg.module(modName)) getOrElse {
      log.log(s"Error: $pkgFile contains no module declaration for $modFile")
      new Module(pkg, modName, modFile.getParent, pkg.source, cfg) // fake it!
    }
    // if we're in the top-level of a multi-module package, we'll have no module; in that case we
    // fake up a default module to avoid a bunch of special casery below
    else Option(pkg.module(Module.DEFAULT)) getOrElse {
      new Module(pkg, Module.DEFAULT, rootPath, pkg.source, cfg)
    }
    modV() = mod

    // init our JavaComponent
    val targetDir = rootPath.resolve("target")
    val classesDir = targetDir.resolve("classes")
    val classpath = classesDir +: moddeps.dependClasspath.toSeqV
    javaComp.javaMetaV() = javaComp.javaMetaV().copy(
      classes = Seq(classesDir),
      outputDir = classesDir,
      buildClasspath = classpath,
      execClasspath = classpath
    )
    javaComp.addTesters()

    // add dirs to our ignores
    val igns = FileProject.stockIgnores
    igns += FileProject.ignorePath(targetDir, root.path)
    ignores() = igns

    // TODO: this is expensive, can we do something cheaper
    val ssum = summarizeSources
    // TODO: do we want to try to support multi-lingual projects? that sounds like a giant PITA,
    // but we could probably at least generate a warning if we have some crazy mishmash of sources
    // TEMP: if we have any Kotlin files, we just use the KotlinCompiler
    if (ssum.contains("kt")) {
      addComponent(classOf[Compiler], new KotlinCompiler(this, javaComp) {
        override def javacOpts = pkg.jcopts.toSeq
        override def kotlincOpts = ScaledProject.this.kotlincOpts
        // override def kotlincVers = ScaledProject.this.kotlincVers
        override protected def willCompile () {
          if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, javaComp.outputDir)
        }
      })

    } else {
      addComponent(classOf[Compiler], new ScalaCompiler(this, javaComp) {
        override def javacOpts = pkg.jcopts.toSeq
        override def scalacOpts = pkg.scopts.toSeq
        override def scalacVers = ScaledProject.this.scalacVers
        override protected def willCompile () {
          if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, javaComp.outputDir)
        }
      })
    }

    metaV() = metaV().copy(
      name = if (mod.isDefault) pkg.name else s"${pkg.name}-${mod.name}",
      ids = Seq(toSrcURL(mod.source)),
      sourceDirs = Seq(rootPath.resolve("src"))
    )
  }

  override def testSeed =
    if (mod.name == "test") None
    else pkg.modules.find(_.name == "test").map(m => {
      val troot = Project.Root(m.root, false) // Scaled projects don't use testMode
      Project.Seed(troot, m.name, true, getClass, List(troot)) })
  override def depends = moddeps.flatten.toSeq.flatMap(toId) :+ platformDepend
  private def platformDepend = Project.PlatformId(Project.JavaPlatform, JDK.thisJDK.majorVersion)

  override def warnings = super.warnings ++ moddeps.flatten.collect {
    case md :Depend.MissingId => s"Missing depend: ${md.id}"
  }

  def resourceDir :Path = rootPath.resolve("src/resources")

  private def scalacVers :String = (depends collectFirst {
    case Project.RepoId(_, "org.scala-lang", "scala-library", version) => version
  }) getOrElse ScalaCompiler.DefaultScalacVersion

  private def kotlincOpts :Seq[String] = Seq()
  // private def kotlincVers :Seq[String] = "" TODO

  private def moddeps :Depends = mod.depends(resolver)
  private val resolver = new Depends.Resolver() {
    import java.util.{List => JList, Optional}
    override def moduleBySource (source :Source) = pspace.knownProjectFor(toSrcURL(source)) match {
      case Some(proj :ScaledProject) => Optional.of(proj.mod)
      case _ =>
        // if this depend is our parent or sibling, we have its module already
        if (source == pkg.source) Optional.ofNullable(pkg.module(Module.DEFAULT))
        else if (source.packageSource == pkg.source) Optional.ofNullable(pkg.module(source.module))
        else Pacman.repo.resolver.moduleBySource(source)
    }
    override def resolve (ids :JList[RepoId]) = Pacman.repo.resolver.resolve(ids)
    override def resolve (id :SystemId) = Pacman.repo.resolver.resolve(id)
    override def isSystem (id :RepoId) = Pacman.repo.resolver.isSystem(id)
    override def systemLoader (path :Path) = Pacman.repo.resolver.systemLoader(path)
  }
}

object ScaledProject {
  import Project.{Id, SrcURL, MavenRepo, RepoId => PRepoId}

  def toSrcURL (src :Source) = SrcURL(src.vcs.toString, src.url.toString)

  def toId (id :Depend.Id) :Option[Id] = id match {
    case rid :RepoId => Some(PRepoId(MavenRepo, rid.groupId, rid.artifactId, rid.version))
    case src :Source => Some(toSrcURL(src))
    case _           => None // SystemId or MissingId, neither of which we handle here
  }

  def findPackage (root :Path, cur :Path) :Path = {
    if (cur == null) throw new IllegalArgumentException("Unable to find 'package.scaled' in root.")
    val pfile = cur.resolve("package.scaled");
    if (Files.exists(pfile)) pfile else findPackage(root, cur.getParent)
  }

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("scaled", true, classOf[ScaledProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, "package.scaled") ||
                                          exists(root, "module.scaled")) 1 else -1
  }
}
