//
// Scaled Package Project plugin - a Scaled extension for handling pacman projects
// https://github.com/scaled/scaled-project/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._
import scaled.pacman._
import scaled.project.Depends

class ScaledDepends (project :Project, val pkg :Package, val mod :Module) extends Depends(project) {
  import Scaled._

  override def ids = moddeps.flatten.toSeq.flatMap(toId) :+
    Project.PlatformId(Project.JavaPlatform, JDK.thisJDK.majorVersion)

  override def warnings = Seq() ++ moddeps.flatten.collect {
    case md :Depend.MissingId => s"Missing depend: ${md.id}"
  }

  def artifactVers (groupId :String, artifactId :String, defvers :String) =
    (ids collectFirst {
      case Project.RepoId(_, gid, aid, vers) if (gid == groupId && aid == artifactId) => vers
    }) getOrElse defvers

  def moddeps = mod.depends(resolver)

  private val resolver = new Depends.Resolver() {
    import java.util.{List => JList, Optional}
    override def ignoreModuleJar = true
    override def moduleBySource (src :Source) = {
      project.pspace.knownProjectFor(toSrcURL(src)).map(_.depends) match {
        case Some(sd :ScaledDepends) => Optional.of(sd.mod)
        case _ =>
          // if this depend is our parent or sibling, we have its module already
          if (src == pkg.source) Optional.ofNullable(pkg.module(Module.DEFAULT))
          else if (src.packageSource == pkg.source) Optional.ofNullable(pkg.module(src.module))
          else Pacman.repo.resolver.moduleBySource(src)
      }
    }
    override def resolve (ids :JList[RepoId]) = Pacman.repo.resolver.resolve(ids)
    override def resolve (id :SystemId) = Pacman.repo.resolver.resolve(id)
    override def isSystem (id :RepoId) = Pacman.repo.resolver.isSystem(id)
    override def systemLoader (path :Path) = Pacman.repo.resolver.systemLoader(path)
  }
}
