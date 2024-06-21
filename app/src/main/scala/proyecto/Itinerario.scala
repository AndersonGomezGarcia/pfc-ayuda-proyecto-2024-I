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
      itinerario.zipWithIndex.map { case (vuelo, index) =>
        calcularTiempoVuelo(vuelo, aeropuerto) + (if (index < itinerario.length - 1) calcularTiempoEspera(vuelo, itinerario(index + 1), aeropuerto) else 0)
      }.sum
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

      (itinerario.map(_.Esc).sum) + (itinerario.size - 1)
    }

    (origen: String, destino: String) => {
      val itinerariosEncontrados = itinerarios(vuelos, aeropuertos)(origen, destino)
      itinerariosEncontrados.sortBy(escalas).take(3)
    }
  }



  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def gmtOffset(cod: String): Double = {
      aeropuertos.find(_.Cod.equalsIgnoreCase(cod)).map(_.GMT).getOrElse(0.0)
    }
    def calcularTiempoVuelo(vuelo: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val gmtOrg = gmtOffset(vuelo.Org)
      val gmtDst = gmtOffset(vuelo.Dst)
      val horaSalidaGMT = (vuelo.HS * 60) + vuelo.MS - ((gmtOrg / 100) * 60).toInt
      val horaLlegadaGMT = (vuelo.HL * 60) + vuelo.ML - ((gmtDst / 100) * 60).toInt
      val duracionVuelo = horaLlegadaGMT - horaSalidaGMT
      if (duracionVuelo < 0) duracionVuelo + 1440 else duracionVuelo
    }

    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuerto: List[Aeropuerto]): Int = {
      itinerario.map(vuelo => calcularTiempoVuelo(vuelo, aeropuerto)).sum
    }
    (cod1: String, cod2: String) => {
      val allItineraries = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      allItineraries.sortBy(itinerary => calcularTiempoTotal(itinerary, aeropuertos)).take(3)
      /*val AllItinerariesTime = allItineraries.map(itinerary => (itinerary, calcularTiempoTotal(itinerary, aeropuertos)))
      AllItinerariesTime.sortBy(_._2).map(_._1).take(3)*/
    }
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    val buscarItinerariosFn = itinerarios(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def calcularLapsoTiempo(horaLlegada: Int, horaCita: Int): Int = {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    def esValido(itinerario: List[Vuelo], tiempoCita: Int): Boolean = {
      val horaLlegada = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerarios = buscarItinerariosFn(origen, destino)
      val itinerariosValidos = todosItinerarios.filter(it => esValido(it, tiempoCita))

      val itinerariosOrdenados = itinerariosValidos.sortBy { it =>
        val horaLlegada = convertirAMinutos(it.last.HL, it.last.ML)
        val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
        val horaSalida = convertirAMinutos(it.head.HS, it.head.MS)
        (lapsoTiempo, horaSalida)
      }

      itinerariosOrdenados.headOption.getOrElse(List.empty)
    }
  }
}