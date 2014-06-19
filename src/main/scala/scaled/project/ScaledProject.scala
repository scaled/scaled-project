//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman._
import scaled.util.Close

class ScaledProject (val root :Path, msvc :MetaService) extends AbstractJavaProject(msvc) {
  import ScaledProject._
  import scala.collection.convert.WrapAsScala._

  private[this] val pkgFile = findPackage(root, root)
  private[this] val modFile = root.resolve("module.scaled") // may not exist
  private[this] val _mod = new Close.Ref[Module](toClose) {
    protected def create = {
      val pkg = new Package(pkgFile)
      if (pkgFile.getParent != root) pkg.module(root.getFileName.toString)
      // if we're in the top-level of a multi-module package, we'll have no module; in that case
      // we fake up a default module to avoid a bunch of special casery below
      else Option(pkg.module(Module.DEFAULT)) getOrElse {
        val cfg = new pacman.Config(java.util.Collections.emptyList[String]())
        new Module(pkg, Module.DEFAULT, root, pkg.source, cfg)
      }
    }
  }
  def pkg :Package = mod.pkg
  def mod :Module = _mod.get

  // hibernate if package.scaled (or module.scaled) changes, which will trigger reload
  { val watchSvc = metaSvc.service[WatchService]
    watchSvc.watchFile(pkgFile, file => hibernate())
    if (Files.exists(modFile)) watchSvc.watchFile(modFile, file => hibernate())
  }
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override def name = if (mod.isDefault) pkg.name else s"${pkg.name}-${mod.name}"
  override def ids = Seq(toSrcURL(mod.source))
  override def depends = mod.depends.flatMap(toId)

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

    override protected def willCompile (tests :Boolean) {
      if (tests) copyResources(testResourceDir, testOutputDir)
      else copyResources(resourceDir, outputDir)
    }

    private def copyResources (rsrcDir :Path, outputDir :Path) {
      if (Files.exists(rsrcDir)) Filez.copyAll(rsrcDir, outputDir)
    }
  }

  override protected def dependClasspath (forTest :Boolean) :Seq[Path] =
    if (forTest) Pacman.repo.createLoader(mod, true).classpath
    else mod.loader(Pacman.repo).classpath

  // TODO: we want to route through project service to find projects kwown thereto, but that means
  // we have to reimplement the package deps + maven deps + system deps blah blah that pacman does
  // (or factor and complexify it so that we can reuse it)

  //   mod.depends flatMap { dep =>
  //     toId(dep).flatMap(classpathById) orElse classpathForSysDep(dep)
  //   }

  // private def classpathById (id :Project.Id) :Option[Path] = projectSvc.projectFor(id) match {
  //   case Some(proj :JavaProject) => Some(proj.classes)
  //   case p                       => println(s"What to do? $id -> $p"); None
  // }

  // private def classpathForSysDep (dep :Depend) :Option[Path] = dep.id match {
  //   case sysId :SystemId => Some(Pacman.repo.sys.resolve(sysId))
  //   case _               => None
  // }
}

object ScaledProject {
  import Project.{Id, SrcURL, MavenRepo, RepoId => PRepoId}

  def toSrcURL (src :Source) = SrcURL(src.vcs.toString, src.url.toString)

  def toId (depend :Depend) :Option[Id] = depend.id match {
    case rid :RepoId => Some(PRepoId(MavenRepo, rid.groupId, rid.artifactId, rid.version))
    case src :Source => Some(toSrcURL(src))
    case _           => None // TODO: SystemId?
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
