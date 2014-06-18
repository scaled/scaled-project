//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

// import codex.extract.JavaExtractor
// import codex.model.Source
// import codex.store.{EphemeralStore, ProjectStore}
import com.google.common.collect.{Multimap, HashMultimap}
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, SimpleFileVisitor}
import reactual.Future
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.pacman._
import scaled.util.{BufferBuilder, Close}

class ScaledProject (val root :Path, msvc :MetaService, projectSvc :ProjectService)
    extends AbstractFileProject(msvc) with JavaProject {
  import ScaledProject._
  import scala.collection.convert.WrapAsScala._

  private[this] val pkgFile = findPackage(root, root)
  private[this] val modFile = root.resolve("module.scaled") // may not exist
  private[this] val _mod = new Close.Ref[Module](toClose) {
    protected def create = {
      val pkg = new Package(pkgFile)
      if (pkgFile.getParent == root) pkg.module(Module.DEFAULT)
      else pkg.module(root.getFileName.toString)
    }
  }
  def pkg :Package = mod.pkg
  def mod :Module = _mod.get

  // hibernate if package.scaled (or module.scaled) changes, which will trigger reload
  metaSvc.service[WatchService].watchFile(pkgFile, file => hibernate())
  if (Files.exists(modFile)) metaSvc.service[WatchService].watchFile(modFile, file => hibernate())
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  def sourceDirs :Seq[Path] = Seq(root.resolve("src/main"))
  def testSourceDirs :Seq[Path] = Seq(root.resolve("src/test"))
  def allSourceDirs = sourceDirs ++ testSourceDirs

  def outputDir :Path = root.resolve("target/classes")
  def testOutputDir :Path = root.resolve("target/test-classes")

  def buildClasspath :Seq[Path] = outputDir +: classpath(false)
  def testClasspath :Seq[Path] = testOutputDir +: outputDir +: classpath(true)

  override def name = if (mod.isDefault) pkg.name else s"${pkg.name}-${mod.name}"
  override def ids = Seq(toSrcURL(mod.source))
  override def depends = mod.depends.flatMap(toId)

  override def describeSelf (bb :BufferBuilder) {
    super.describeSelf(bb)

    bb.addSubHeader("Scaled Info")
    bb.addSection("Source dirs:")
    bb.addKeysValues("compile: " -> sourceDirs.mkString(" "),
                     "test: "    -> testSourceDirs.mkString(" "))
    // val srcsum = summarizeSources(true)
    // if (!srcsum.isEmpty) {
    //   bb.addSection("Source files:")
    //   bb.addKeysValues(srcsum.asMap.entrySet.map(
    //     e => (s".${e.getKey}: ", e.getValue.size.toString)).toSeq :_*)
    // }
    bb.addSection("Output dirs:")
    bb.addKeysValues("compile: " -> outputDir.toString,
                     "test: "    -> testOutputDir.toString)
    bb.addSection("Compile classpath:")
    buildClasspath foreach { p => bb.add(p.toString) }
    bb.addSection("Test classpath:")
    testClasspath foreach { p => bb.add(p.toString) }
  }

  // tell other Java projects where to find our compiled classes
  override def classes = outputDir

  override protected def ignores = FileProject.stockIgnores ++ Set("target")

  // TODO: use summarizeSources to determine whether to use a Java or Scala compiler
  override protected def createCompiler () = new ScalaCompiler(this) {
    override def sourceDirs = ScaledProject.this.sourceDirs
    override def buildClasspath = ScaledProject.this.buildClasspath
    override def outputDir = ScaledProject.this.outputDir
    override def testSourceDirs = ScaledProject.this.testSourceDirs
    override def testClasspath = ScaledProject.this.testClasspath
    override def testOutputDir = ScaledProject.this.testOutputDir
  }

  // TODO: how to determine what kind of tester to use?
  override protected def createTester () :Tester = new JUnitTester(this) {
    override def testSourceDirs = ScaledProject.this.testSourceDirs
    override def testOutputDir = ScaledProject.this.testOutputDir
    override def testClasspath = ScaledProject.this.testClasspath
  }

  override protected def createRunner () = new JavaRunner(this) {
    override def execClasspath = buildClasspath // TODO
  }

  // TODO: handle forTest
  private def classpath (forTest :Boolean) :Seq[Path] = mod.loader(Pacman.repo).classpath
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
