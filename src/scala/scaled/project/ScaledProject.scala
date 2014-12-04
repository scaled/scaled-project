//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman._
import scaled.util.Close

class ScaledProject (val root :Project.Root, ps :ProjectSpace) extends AbstractJavaProject(ps) {
  import ScaledProject._

  private[this] val pkgFile = findPackage(rootPath, rootPath)
  private[this] val modFile = rootPath.resolve("module.scaled") // may not exist
  private[this] val _mod = new Close.Ref[Module](toClose) {
    protected def create = {
      val pkg = new Package(pkgFile) ; val modname = rootPath.getFileName.toString
      if (pkgFile.getParent != rootPath) Option(pkg.module(modname)) getOrElse {
        log.log(s"Error: $pkgFile contains no module declaration for $modFile")
        new Module(pkg, modname, modFile.getParent, pkg.source, cfg) // fake it!
      }
      // if we're in the top-level of a multi-module package, we'll have no module; in that case
      // we fake up a default module to avoid a bunch of special casery below
      else Option(pkg.module(Module.DEFAULT)) getOrElse {
        new Module(pkg, Module.DEFAULT, rootPath, pkg.source, cfg)
      }
    }
    private def cfg = new pacman.Config(Seq.empty[String].asJList)
  }

  private def rootPath = root.path
  def mod :Module = _mod.get
  def pkg :Package = mod.pkg

  // hibernate if package.scaled (or module.scaled) changes, which will trigger reload
  { val watchSvc = metaSvc.service[WatchService]
    watchSvc.watchFile(pkgFile, file => hibernate())
    if (Files.exists(modFile)) watchSvc.watchFile(modFile, file => hibernate())
  }
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = if (mod.isDefault) pkg.name else s"${pkg.name}-${mod.name}"
  override def idName = s"scaled-$name" // TODO: use munged src url?
  override def ids = Seq(toSrcURL(mod.source))
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

  override def sourceDirs :Seq[Path] = Seq(rootPath.resolve("src"))
  override def outputDir :Path = rootPath.resolve("target/classes")

  def resourceDir :Path = rootPath.resolve("src/main/resources")

  override protected def ignores = FileProject.stockIgnores ++ Set("target")

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = ScaledProject.this.sourceDirs
    override def buildClasspath = ScaledProject.this.buildClasspath
    override def outputDir = ScaledProject.this.outputDir

    override def javacOpts = pkg.jcopts.toSeq
    override def scalacOpts = pkg.scopts.toSeq

    override protected def willCompile () {
      if (Files.exists(resourceDir)) Filez.copyAll(resourceDir, outputDir)
    }
  }

  override protected def buildDependClasspath = moddeps.dependClasspath.toSeqV
  override protected def execDependClasspath = buildDependClasspath

  // scaled projects don't have a magical test subproject; tests are in a top-level project
  // (usually a module named tests or test)
  override protected def createTester () :Tester = new JUnitTester(this) {
    override def testSourceDirs = sourceDirs
    override def testOutputDir = outputDir
    override def testClasspath = buildClasspath
  }

  private def moddeps :Depends = mod.depends(resolver)
  private val resolver = new Depends.Resolver() {
    import java.util.{List => JList, Optional}
    override def moduleBySource (source :Source) = pspace.projectFor(toSrcURL(source)) match {
      case Some(proj :ScaledProject) => Optional.of(proj.mod)
      case _ => Pacman.repo.resolver.moduleBySource(source)
    }
    override def resolve (ids :JList[RepoId]) = Pacman.repo.resolver.resolve(ids)
    override def resolve (id :SystemId) = Pacman.repo.resolver.resolve(id)
    override def isShared (id :RepoId) = Pacman.repo.resolver.isShared(id)
    override def sharedLoader (path :Path) = Pacman.repo.resolver.sharedLoader(path)
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
