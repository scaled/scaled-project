//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman._
import scaled.util.Close

class ScaledProject (val root :Path, ps :ProjectSpace) extends AbstractJavaProject(ps) {
  import ScaledProject._
  import scala.collection.convert.WrapAsScala._

  private[this] val pkgFile = findPackage(root, root)
  private[this] val modFile = root.resolve("module.scaled") // may not exist
  private[this] val _mod = new Close.Ref[Module](toClose) {
    protected def create = {
      val pkg = new Package(pkgFile) ; val modname = root.getFileName.toString
      if (pkgFile.getParent != root) Option(pkg.module(modname)) getOrElse {
        log.log(s"Error: $pkgFile contains no module declaration for $modFile")
        new Module(pkg, modname, modFile.getParent, pkg.source, cfg) // fake it!
      }
      // if we're in the top-level of a multi-module package, we'll have no module; in that case
      // we fake up a default module to avoid a bunch of special casery below
      else Option(pkg.module(Module.DEFAULT)) getOrElse {
        new Module(pkg, Module.DEFAULT, root, pkg.source, cfg)
      }
    }
    private def cfg = new pacman.Config(java.util.Collections.emptyList[String]())
  }
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
  override def depends = moddeps(false).flatten.map(toId).flatten :+ platformDepend
  private def platformDepend = Project.PlatformId(Project.JavaPlatform, JDK.thisJDK.majorVersion)

  override def warnings = super.warnings ++ moddeps(false).flatten.collect {
    case md :Depend.MissingId => s"Missing depend: ${md.id}"
  }

  override def sourceDirs :Seq[Path] = Seq(root.resolve("src/main"))
  override def testSourceDirs :Seq[Path] = Seq(root.resolve("src/test"))

  override def outputDir :Path = root.resolve("target/classes")
  override def testOutputDir :Path = root.resolve("target/test-classes")

  def resourceDir :Path = root.resolve("src/main/resources")
  def testResourceDir :Path = root.resolve("src/test/resources")

  override protected def ignores = FileProject.stockIgnores ++ Set("target")

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = ScaledProject.this.sourceDirs
    override def buildClasspath = ScaledProject.this.buildClasspath
    override def outputDir = ScaledProject.this.outputDir
    override def testSourceDirs = ScaledProject.this.testSourceDirs
    override def testClasspath = ScaledProject.this.testClasspath
    override def testOutputDir = ScaledProject.this.testOutputDir

    override def javacOpts = pkg.jcopts
    override def scalacOpts = pkg.scopts

    override protected def willCompile (tests :Boolean) {
      if (tests) copyResources(testResourceDir, testOutputDir)
      else copyResources(resourceDir, outputDir)
    }

    private def copyResources (rsrcDir :Path, outputDir :Path) {
      if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, outputDir)
    }
  }

  override protected def buildDependClasspath = moddeps(false).dependClasspath
  override protected def testDependClasspath = moddeps(true).dependClasspath
  override protected def execDependClasspath = buildDependClasspath

  private def moddeps (forTest :Boolean) = mod.depends(resolver, forTest)
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
