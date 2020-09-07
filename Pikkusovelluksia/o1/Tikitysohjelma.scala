package o1

// Tämä koodi liittyy lukuun 3.1 ja on esitelty siellä.

object Tikitysohjelma extends App {
  val tikityslaskuri = new Laskuri(0)
  val tausta = rectangle(500, 500, Black)
  val face = Pic("face.png")

  val nakyma = new View(tikityslaskuri, 50) {
    def makePic = tausta.place(rectangle(tikityslaskuri.arvo, tikityslaskuri.arvo, White).clockwise(tikityslaskuri.arvo), new Pos(250, 250))    
    //def makePic = tausta.place(face.scaleTo(tikityslaskuri.arvo, tikityslaskuri.arvo).clockwise(tikityslaskuri.arvo), new Pos(250, 250))

    override def onTick() = {
      tikityslaskuri.etene()
      
    }
  }

  nakyma.start()
}
