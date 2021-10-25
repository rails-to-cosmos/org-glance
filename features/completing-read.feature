Feature: Completing read
  In order to extract information from things as fast as possible
  As a user
  I want to run completing read on subsets of things

  Scenario: Link in title should be replaced with link's description
    When I capture thing "[[https://www.youtube.com/watch?v=bYy90EUAh98][Episode]]" of class "task"
    And I materialize "Episode" of class "task"
    Then I should be in the buffer "org-glance:<Episode>"
