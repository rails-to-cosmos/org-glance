Feature: Overview
  In order to manage things
  As a user
  I want to manage things in class dimension

  Scenario: Create simple class overview
    When I capture thing "Queen Concert" of class "show"
    Then I should have 1 active class
    And I should have 1 thing of class "show" registered
    When I request "show" overview
    Then I am in "show" overview buffer
    And there is 1 headline here

  Scenario: Kill headline from overview
    When I capture thing "Informative YouTube Video!!1" of class "video"
    When I capture thing "Educative Article" of class "article"

    And I should have 2 active classes
    And I should have 1 thing of class "video" registered
    And I should have 1 thing of class "article" registered

    When I request "video" overview
    When I jump to the first headline
    And I kill headline at point
    And there are no headlines here

    And I should have 2 active classes
    And I should have 0 things of class "video" registered
    And I should have 1 thing of class "article" registered

  Scenario: Kill should be case-independent
    When I capture thing "javascript video" of class "video"

    And I should have 1 active class
    And I should have 1 thing of class "video" registered

    When I request "video" overview
    And I jump to the first headline
    And I materialize original headline from overview
    And I change tag "video" to "VIDEO"
    And I kill current buffer
    And I should have 1 active classes
    And I should have 1 thing of class "video" registered
    And I kill headline at point
    And there are no headlines here
    And I should have 1 active classes
    And I should have 0 things of class "video" registered

  Scenario: Basic overview materialization
    When I capture thing "Picnic" of class "event"
    And I request "event" overview
    And I jump to the first headline
    And I materialize original headline from overview
    Then I should be in the buffer "org-glance:<Picnic>"

  Scenario: Overview materialization should reuse material buffer
    When I capture thing "Picnic" of class "event"
    And I request "event" overview
    And I jump to the first headline
    And I materialize original headline from overview
    Then I should be in the buffer "org-glance:<Picnic>"
    And I request "event" overview
    And I materialize original headline from overview
    Then I should be in the buffer "org-glance:<Picnic>"
