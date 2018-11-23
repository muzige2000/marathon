package mesosphere.mesos

import mesosphere.marathon.tasks.ResourceUtil
import mesosphere.mesos.protos.{Resource, ResourceProviderID}
import org.apache.mesos.Protos
import org.apache.mesos.Protos.Resource.ReservationInfo

import scala.collection.immutable.Seq

sealed trait GpuSetMatch extends ScalarMatchResult {

  def consumedResources: Seq[Protos.Resource]

  def roles: Seq[String]

  def consumed: Seq[GpuSetMatchResult.Consumption]
}

object GpuSetMatchResult {

  sealed trait Scope {
    def note: String = ""
  }

  object Scope {

    case object NoneGpu extends Scope

  }

  trait Consumption

}

/** An unsuccessful match of a scalar resource. */
case class GpuSetNoMatch(resourceName: String, requiredValue: Double, offeredGpus: Double, scope: GpuSetMatchResult.Scope)
  extends GpuSetMatch {

  require(resourceName == Resource.GPUS)
  require(requiredValue > offeredGpus)

  def matches: Boolean = false

  def roles: Seq[String] = Nil

  override def toString: String = {
    s"$resourceName${scope.note} NOT SATISFIED ($requiredValue > $offeredGpus)"
  }

  def consumed: Seq[GpuSetMatchResult.Consumption] = Nil

  override def consumedResources: Seq[Protos.Resource] = Nil
}

/** A successful match of a scalar resource requirement. */
case class KakaoBrainGpuSetMatch(
    resourceName: String, requiredValue: Double,
    consumed: Seq[KakaoBrainGpuSetMatch.Consumption], scope: GpuSetMatchResult.Scope) extends GpuSetMatch {

  final val matches = true

  require(resourceName == Resource.GPUS, "GPUS is used for gpu-set resources")
  require(consumedGpuSet.size >= requiredValue)

  def consumedResources: Seq[Protos.Resource] = {
    consumed.map {
      case KakaoBrainGpuSetMatch.Consumption(value, role, providerId, reservation) =>
        ResourceUtil.BuildSetResource("gpu-set", consumedGpuSet)
    }
  }

  def roles: Seq[String] = consumed.map(_.role)

  lazy val consumedGpuSet: Set[String] = consumed.flatMap(_.consumedGpuSet).toSet

  override def toString: String = {
    s"$resourceName${scope.note} SATISFIED ($requiredValue <= ${consumedGpuSet.size})"
  }
}

object KakaoBrainGpuSetMatch {

  case class Consumption(consumedGpuSet: Set[String], role: String,
      providerId: Option[ResourceProviderID], reservation: Option[ReservationInfo]) extends GpuSetMatchResult.Consumption

}

