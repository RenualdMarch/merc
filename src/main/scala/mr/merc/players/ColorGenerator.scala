package mr.merc.players

import scala.util.Random
import scalafx.scene.paint.Color

class ColorGenerator {

  private var returnedColors: Set[Color] = Set()

  private var stage = 1d

  private def currentStep = 1 / stage

  private def isOne(a: Double) = math.abs(1 - a) <= 10e-10

  private var r = 0d
  private var g = 0d
  private var b = 0d

  def nextColor(): Color = {
    val result = Color.color(r, g, b)
    if (isOne(r)) {
      if (isOne(g)) {
        if (isOne(b)) {
          stage += 1
          r = 0
          g = 0
          b = 0
        } else {
          b += currentStep
          r = 0
          g = 0
        }
      } else {
        g += currentStep
        r = 0
      }
    } else {
      r += currentStep
    }
    if (returnedColors.contains(result)) {
      nextColor()
    } else {
      returnedColors += result
      result
    }
  }
}

class CyclicColorGeneratorWithNames {
  private val colors: Vector[(String, Color)] = Random.shuffle(Vector(
    "AliceBlue" -> Color.AliceBlue,
    "AntiqueWhite" -> Color.AntiqueWhite,
    "Aqua" -> Color.Aqua,
    "Aquamarine" -> Color.Aquamarine,
    "Azure" -> Color.Azure,
    "Beige" -> Color.Beige,
    "Bisque" -> Color.Bisque,
    "Black" -> Color.Black,
    "BlanchedAlmond" -> Color.BlanchedAlmond,
    "Blue" -> Color.Blue,
    "BlueViolet" -> Color.BlueViolet,
    "Brown" -> Color.Brown,
    "Burlywood" -> Color.Burlywood,
    "CadetBlue" -> Color.CadetBlue,
    "Chartreuse" -> Color.Chartreuse,
    "Chocolate" -> Color.Chocolate,
    "Coral" -> Color.Coral,
    "CornflowerBlue" -> Color.CornflowerBlue,
    "Cornsilk" -> Color.Cornsilk,
    "Crimson" -> Color.Crimson,
    "Cyan" -> Color.Cyan,
    "DarkBlue" -> Color.DarkBlue,
    "DarkCyan" -> Color.DarkCyan,
    "DarkGoldenrod" -> Color.DarkGoldenrod,
    "DarkGray" -> Color.DarkGray,
    "DarkGreen" -> Color.DarkGreen,
    "DarkGrey" -> Color.DarkGrey,
    "DarkKhaki" -> Color.DarkKhaki,
    "DarkMagenta" -> Color.DarkMagenta,
    "DarkOliveGreen" -> Color.DarkOliveGreen,
    "DarkOrange" -> Color.DarkOrange,
    "DarkOrchid" -> Color.DarkOrchid,
    "DarkRed" -> Color.DarkRed,
    "DarkSalmon" -> Color.DarkSalmon,
    "DarkSeaGreen" -> Color.DarkSeaGreen,
    "DarkSlateBlue" -> Color.DarkSlateBlue,
    "DarkSlateGray" -> Color.DarkSlateGray,
    "DarkSlateGrey" -> Color.DarkSlateGrey,
    "DarkTurquoise" -> Color.DarkTurquoise,
    "DarkViolet" -> Color.DarkViolet,
    "DeepPink" -> Color.DeepPink,
    "DeepSkyBlue" -> Color.DeepSkyBlue,
    "DimGray" -> Color.DimGray,
    "DimGrey" -> Color.DimGrey,
    "DodgerBlue" -> Color.DodgerBlue,
    "FireBrick" -> Color.FireBrick,
    "FloralWhite" -> Color.FloralWhite,
    "ForestGreen" -> Color.ForestGreen,
    "Fuchsia" -> Color.Fuchsia,
    "Gainsboro" -> Color.Gainsboro,
    "GhostWhite" -> Color.GhostWhite,
    "Gold" -> Color.Gold,
    "Goldenrod" -> Color.Goldenrod,
    "Gray" -> Color.Gray,
    "Green" -> Color.Green,
    "GreenYellow" -> Color.GreenYellow,
    "Grey" -> Color.Grey,
    "Honeydew" -> Color.Honeydew,
    "HotPink" -> Color.HotPink,
    "IndianRed" -> Color.IndianRed,
    "Indigo" -> Color.Indigo,
    "Ivory" -> Color.Ivory,
    "Khaki" -> Color.Khaki,
    "Lavender" -> Color.Lavender,
    "LavenderBlush" -> Color.LavenderBlush,
    "LawnGreen" -> Color.LawnGreen,
    "LemonChiffon" -> Color.LemonChiffon,
    "LightBlue" -> Color.LightBlue,
    "LightCoral" -> Color.LightCoral,
    "LightCyan" -> Color.LightCyan,
    "LightGoldrenrodYellow" -> Color.LightGoldrenrodYellow,
    "LightGray" -> Color.LightGray,
    "LightGreen" -> Color.LightGreen,
    "LightGrey" -> Color.LightGrey,
    "LightPink" -> Color.LightPink,
    "LightSalmon" -> Color.LightSalmon,
    "LightSeaGreen" -> Color.LightSeaGreen,
    "LightSkyBlue" -> Color.LightSkyBlue,
    "LightSlateGray" -> Color.LightSlateGray,
    "LightSlateGrey" -> Color.LightSlateGrey,
    "LightSteelBlue" -> Color.LightSteelBlue,
    "LightYellow" -> Color.LightYellow,
    "Lime" -> Color.Lime,
    "LimeGreen" -> Color.LimeGreen,
    "Linen" -> Color.Linen,
    "Magenta" -> Color.Magenta,
    "Maroon" -> Color.Maroon,
    "MediumAquamarine" -> Color.MediumAquamarine,
    "MediumBlue" -> Color.MediumBlue,
    "MediumOrchid" -> Color.MediumOrchid,
    "MediumPurple" -> Color.MediumPurple,
    "MediumSeaGreen" -> Color.MediumSeaGreen,
    "MediumSlateBlue" -> Color.MediumSlateBlue,
    "MediumSpringGreen" -> Color.MediumSpringGreen,
    "MediumTurquoise" -> Color.MediumTurquoise,
    "MediumVioletRed" -> Color.MediumVioletRed,
    "MidnightBlue" -> Color.MidnightBlue,
    "MintCream" -> Color.MintCream,
    "MistyRose" -> Color.MistyRose,
    "Moccasin" -> Color.Moccasin,
    "NavajoWhite" -> Color.NavajoWhite,
    "Navy" -> Color.Navy,
    "OldLace" -> Color.OldLace,
    "Olive" -> Color.Olive,
    "OliveDrab" -> Color.OliveDrab,
    "Orange" -> Color.Orange,
    "OrangeRed" -> Color.OrangeRed,
    "Orchid" -> Color.Orchid,
    "PaleGoldrenrod" -> Color.PaleGoldrenrod,
    "PaleGreen" -> Color.PaleGreen,
    "PaleTurquoise" -> Color.PaleTurquoise,
    "PaleVioletRed" -> Color.PaleVioletRed,
    "PapayaWhip" -> Color.PapayaWhip,
    "PeachPuff" -> Color.PeachPuff,
    "Peru" -> Color.Peru,
    "Pink" -> Color.Pink,
    "Plum" -> Color.Plum,
    "PowderBlue" -> Color.PowderBlue,
    "Purple" -> Color.Purple,
    "Red" -> Color.Red,
    "RosyBrown" -> Color.RosyBrown,
    "RoyalBlue" -> Color.RoyalBlue,
    "SaddleBrown" -> Color.SaddleBrown,
    "Salmon" -> Color.Salmon,
    "SandyBrown" -> Color.SandyBrown,
    "SeaGreen" -> Color.SeaGreen,
    "SeaShell" -> Color.SeaShell,
    "Sienna" -> Color.Sienna,
    "Silver" -> Color.Silver,
    "SkyBlue" -> Color.SkyBlue,
    "SlateBlue" -> Color.SlateBlue,
    "SlateGray" -> Color.SlateGray,
    "SlateGrey" -> Color.SlateGrey,
    "Snow" -> Color.Snow,
    "SpringGreen" -> Color.SpringGreen,
    "SteelBlue" -> Color.SteelBlue,
    "Tan" -> Color.Tan,
    "Teal" -> Color.Teal,
    "Thistle" -> Color.Thistle,
    "Tomato" -> Color.Tomato,
    "Turquoise" -> Color.Turquoise,
    "Violet" -> Color.Violet,
    "Wheat" -> Color.Wheat,
    "White" -> Color.White,
    "WhiteSmoke" -> Color.WhiteSmoke,
    "Yellow" -> Color.Yellow,
    "YellowGreen" -> Color.YellowGreen
  ))

  private var counter = 0

  def nextNameAndColor(): (String, Color) = {
    val result = colors(counter)
    if (counter + 1 == colors.size) {
      counter = 0
    } else {
      counter += 1
    }
    result
  }
}

class BestColorsGeneratorWithNames {

  private val colors: Vector[(String, Color)] = Random.shuffle(Vector(
    "White" -> Color.White,
    "Black" -> Color.Black,
    "Red" -> Color.Red,
    "Green" -> Color.Green,
    "Blue" -> Color.Blue,
    "Yellow" -> Color.Yellow,
    "Magenta" -> Color.Magenta,
    "Cyan" -> Color.Cyan
  ))

  private var counter = 0

  def nextNameAndColor(): (String, Color) = {
    val result = colors(counter)
    if (counter + 1 == colors.size) {
      counter = 0
    } else {
      counter += 1
    }
    result
  }
}