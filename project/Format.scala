object Format {
  import com.typesafe.sbt.SbtScalariform._
 
  lazy val settings = scalariformSettings ++ Seq(
    ScalariformKeys.preferences := formattingPreferences
  )
 
  lazy val formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences().
      setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true).
      setPreference(DoubleIndentClassDeclaration, true)
  }
}
