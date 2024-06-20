package proyecto

class Itinerario() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]


  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
      origen match {
        case o if o == destino => List(List())
        case o if visitados.contains(o) => List()
        case _ =>
          val vuelosDesdeOrigen = vuelos.filter(_.Org == origen)
          val nuevasVisitados = visitados + origen

          vuelosDesdeOrigen.flatMap { vuelo =>
            val rutasDesdeDestino = buscarItinerarios(vuelo.Dst, destino, nuevasVisitados)
            rutasDesdeDestino.map(ruta => vuelo :: ruta)
          }
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2)
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje

    def gmtOffset(cod: String): Double = {
      aeropuertos.find(_.Cod.equalsIgnoreCase(cod)).map(_.GMT).getOrElse(0.0)
    }
    def calcularTiempoVuelo(vuelo: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val gmtOrg = gmtOffset(vuelo.Org)
      val gmtDst = gmtOffset(vuelo.Dst)
      val horaSalidaGMT = vuelo.HS * 60 + vuelo.MS - ((gmtOrg / 100) * 60).toInt
      val horaLlegadaGMT = vuelo.HL * 60 + vuelo.ML - ((gmtDst / 100) * 60).toInt
      val duracionVuelo = horaLlegadaGMT - horaSalidaGMT
      if (duracionVuelo < 0) duracionVuelo + 1440 else duracionVuelo
    }

    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val horaLlegada = (vuelo1.HL  * 60) + vuelo1.ML
      val horaSalida = (vuelo2.HS * 60) + vuelo2.MS
      if (horaSalida < horaLlegada) (((24 * 60) - horaLlegada) + horaSalida)
      else (horaSalida - horaLlegada)
    }

    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuerto: List[Aeropuerto]): Int = {
      itinerario match {
        case Nil => 0
        case vuelo :: Nil => calcularTiempoVuelo(vuelo, aeropuerto)
        case vuelo1 :: vuelo2 :: tail => calcularTiempoVuelo(vuelo1, aeropuerto) + calcularTiempoEspera(vuelo1, vuelo2, aeropuerto) + calcularTiempoTotal(vuelo2 :: tail, aeropuerto)
      }
    }


    (cod1: String, cod2: String) => {
      val allItineraries = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val AllItinerariesTime = allItineraries.map(itinerary => (itinerary, calcularTiempoTotal(itinerary, aeropuertos)))
      //println(AllItinerariesTime(1))
      AllItinerariesTime.sortBy(_._2).map(_._1).take(3)
    }
  }

  def itinerariosEscalas(vuelos: vuelos, aeropuertos: aeropuertos): (String, String) => List[List[Vuelo]] = {
    def escalas(itinerario: List[Vuelo]): Int = {
      itinerario.map(_.Esc).sum + (itinerario.length - 1)
    }

    (origen: String, destino: String) => {
      val itinerariosEncontrados = itinerarios(vuelos, aeropuertos)(origen, destino)
      itinerariosEncontrados.sortBy(escalas).take(3)
    }
  }



  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def calcularTiempoVuelo(vuelo: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val gmtOrg = aeropuerto.find(_.Cod == vuelo.Org).get.GMT
      val gmtDst = aeropuerto.find(_.Cod == vuelo.Dst).get.GMT
      val diferenciaGMT = if (gmtOrg > gmtDst) {
        (gmtOrg - gmtDst) * -60 / 100
      } else {
        (gmtDst - gmtOrg) * 60 / 100
      }
      val horaSalida = (vuelo.HS * 60) + vuelo.MS
      val horaLlegada = (vuelo.HL * 60) + vuelo.ML
      if (horaLlegada < horaSalida) ((((24 * 60) - horaSalida) + horaLlegada) - diferenciaGMT).toInt
      else Math.round((horaLlegada - horaSalida) + diferenciaGMT).toInt
    }
    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuerto: List[Aeropuerto]): Int = {
      itinerario match {
        case Nil => 0
        case vuelo :: Nil => 0
        case vuelo1 :: vuelo2 :: tail => calcularTiempoVuelo(vuelo1, aeropuerto) +  calcularTiempoTotal(vuelo2 :: tail, aeropuerto)
      }
    }


    (cod1: String, cod2: String) => {
      val allItineraries = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val AllItinerariesTime = allItineraries.map(itinerary => (itinerary, calcularTiempoTotal(itinerary, aeropuertos)))
      AllItinerariesTime.sortBy(_._2).map(_._1).take(3)
    }
  }

  def itinerariosSalida(vuelos: vuelos, aeropuertos: aeropuertos): (String, String, Int, Int) => List[List[Vuelo]] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def buscarItinerarios(origen: String, destino: String, aeropuertosVisitados: Set[String], rutaActual: List[Vuelo], horaCita: Int): List[List[Vuelo]] = {
      if (origen == destino) {
        if (convertirAMinutos(rutaActual.last.HL, rutaActual.last.ML) <= horaCita) List(rutaActual)
        else List()
      } else {
        val vuelosDisponibles = vuelos.filter(vuelo =>
          vuelo.Org == origen)
        vuelosDisponibles.flatMap { vuelo =>
          val nuevosAeropuertosVisitados = aeropuertosVisitados + origen
          buscarItinerarios(vuelo.Dst, destino, nuevosAeropuertosVisitados, rutaActual :+ vuelo, horaCita)
        }
      }
    }
    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val rutasEncontradas = vuelos.filter(_.Org == origen).flatMap { vuelo =>
        buscarItinerarios(vuelo.Dst, destino, Set.empty, List(vuelo), tiempoCita)
      }
      if (rutasEncontradas.isEmpty) List()
      else {
        val ultimaSalida = rutasEncontradas.map(ruta => convertirAMinutos(ruta.last.HS, ruta.last.MS)).max
        rutasEncontradas.filter(ruta => convertirAMinutos(ruta.last.HS, ruta.last.MS) == ultimaSalida)
      }
    }
  }
}