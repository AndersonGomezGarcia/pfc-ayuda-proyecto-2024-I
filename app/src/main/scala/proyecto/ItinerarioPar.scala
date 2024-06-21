package proyecto

import common._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.concurrent.Await
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ItinerarioPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
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




  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
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
      val allItineraries = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)

      // Convertimos la colección a una colección paralela
      val allItinerariesPar = allItineraries.par

      // Calculamos el tiempo total en paralelo
      val allItinerariesTime = allItinerariesPar.map(itinerary => (itinerary, calcularTiempoTotal(itinerary, aeropuertos)))

      // Convertimos a lista, ordenamos y seleccionamos los primeros 3
      allItinerariesTime
        .toList
        .sortBy(_._2)
        .map(_._1)
        .take(3)
    }
  }

  def itinerariosEscalasPar(vuelos: vuelos, aeropuertos: aeropuertos): (String, String) => List[List[Vuelo]] = {
    def escalas(itinerario: List[Vuelo]): Int = {
      (itinerario.map(_.Esc).sum) + (itinerario.size - 1)
    }

    (origen: String, destino: String) => {
      val itinerariosEncontrados = itinerariosPar(vuelos, aeropuertos)(origen, destino)

      // Convertimos la colección a una colección paralela
      val itinerariosParalelos = itinerariosEncontrados.par

      // Usamos sortBy y take en la colección paralela
      itinerariosParalelos
        .map(itinerario => (itinerario, escalas(itinerario))) // Calculamos las escalas en paralelo
        .toList // Convertimos a lista para poder usar sortBy
        .sortBy(_._2) // Ordenamos por el número de escalas
        .map(_._1) // Extraemos los itinerarios
        .take(3) // Tomamos los primeros 3
    }
  }



  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
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
      val allItineraries = Await.result(Future { itinerariosPar(vuelos, aeropuertos)(cod1, cod2) }, Duration.Inf)

      val calcularTiemposFuturas = Future.traverse(allItineraries) { itinerario =>
        Future { calcularTiempoTotal(itinerario, aeropuertos) }
      }

      val allItinerariesWithTimes = Await.result(calcularTiemposFuturas, Duration.Inf).zip(allItineraries)

      allItinerariesWithTimes.sortBy(_._1).map(_._2).take(3)
    }
  }



  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    val buscarItinerariosFn = itinerariosPar(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def calcularHoraLlegadaTotal(itinerario: List[Vuelo]): Int = {
      convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
    }

    def calcularHoraSalidaTotal(itinerario: List[Vuelo]): Int = {
      convertirAMinutos(itinerario.head.HS, itinerario.head.MS)
    }

    def calcularLapsoTiempo(horaLlegada: Int, horaCita: Int): Int = {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    def esValido(itinerario: List[Vuelo], tiempoCita: Int): Boolean = {
      val horaLlegada = calcularHoraLlegadaTotal(itinerario)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerariosFuturo = Future { buscarItinerariosFn(origen, destino) }

      val itinerariosValidosFuturo = todosItinerariosFuturo.flatMap { todosItinerarios =>
        Future.sequence(todosItinerarios.map(it => Future {
          if (esValido(it, tiempoCita)) Some(it) else None
        })).map(_.flatten)
      }

      val itinerariosOrdenadosFuturo = itinerariosValidosFuturo.flatMap { itinerariosValidos =>
        Future.sequence(itinerariosValidos.map { it =>
          Future {
            val horaLlegada = calcularHoraLlegadaTotal(it)
            val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
            (it, lapsoTiempo, calcularHoraSalidaTotal(it))
          }
        })
      }.map { itinerariosOrdenados =>
        itinerariosOrdenados.sortBy { case (_, lapsoTiempo, horaSalida) =>
          (lapsoTiempo, horaSalida)
        }
      }

      Await.result(itinerariosOrdenadosFuturo.map(_.headOption.map(_._1).getOrElse(List.empty)), Duration.Inf)
    }
  }
}
