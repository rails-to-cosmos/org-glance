Feature: Overview
  In order to manage things
  As a user
  I want to manage things in class dimension

  Scenario: Overview manipulations
    When I capture thing "Informative YouTube Video!!1" of class "video"
    When I capture thing "Educative Article" of class "article"
    Then I should have 2 active classes
    And I should have 1 thing of class "video" registered
    And I should have 1 thing of class "article" registered

    When I request "video" overview
    Then I am in "video" overview buffer
    And there is 1 headline here

    When I jump to the first headline
    And I kill headline at point
    And there are no headlines here
    And I should have 2 active classes
    And I should have 0 things of class "video" registered
    And I should have 1 thing of class "article" registered

    When I request "article" overview
    And I jump to the first headline
    And I materialize headline at point from overview
    Then I should be in the buffer "org-glance:<Educative Article>"
    And I request "article" overview
    And I materialize headline at point from overview
    Then I should be in the buffer "org-glance:<Educative Article>"
