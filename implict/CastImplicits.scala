object CastImplicits {
    implicit def floatToInt(f: Float) = f.toInt
    implicit def doubleToInt(d: Double) = d.toInt
    implicit def stringToInt(s: String) = s.toInt
}
