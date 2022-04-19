name := """CodeReview"""
organization := "com.optconnect"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.8"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play"           %% "scalatestplus-play"           % "5.1.0"           % Test
libraryDependencies += "com.typesafe.play"                %% "play-json-joda"               % "2.9.2"
libraryDependencies += "org.reactivemongo"                %% "play2-reactivemongo"          % "1.0.6-play28"
libraryDependencies += "org.typelevel"                    %% "squants"                      % "1.8.3"

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"
