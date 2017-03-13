package spinoco.protocol.mgcp

/**
  * Created by pach on 09/03/17.
  */
object ReasonCodeType extends Enumeration {

  val Normal = Value(0)
  val EndpointMalfunction = Value(900)
  val EndpointOutOfService = Value(901)
  val LossOfConnectivity = Value(902)
  val QoSReservationLost = Value(903)
  val ManualIntervention = Value(904)
  val FacilityFailure = Value(905)



}
