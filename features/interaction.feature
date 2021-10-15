@interaction
Feature: Interaction
  In order to use information of things
  As a user
  I want to choose things and interact with it

  Scenario: Grab features of things from metastore
    When I capture thing "Czech Republic" of class "wikipage" with contents
      """
      The Czech Republic, also known by its short-form name
      Czechia and formerly known as Bohemia, is a landlocked country
      in Central Europe. It is bordered by [[https://en.wikipedia.org/wiki/Austria][Austria]] to the south,
      Germany to the west, Poland to the northeast, and Slovakia to
      the east.[14] The Czech Republic has a hilly landscape that
      covers an area of 78,871 square kilometers (30,452 sq mi) with a
      mostly temperate continental and oceanic climate.
      """
    And I materialize "Czech Republic" of class "wikipage"
    Then headline at point should contain links
    And headline at point should not be encrypted
    And headline at point should not contain properties
    And headline at point should not be archived
    And headline at point should not be commented
    And I can open 1 headline
    And I can decrypt 0 headlines
