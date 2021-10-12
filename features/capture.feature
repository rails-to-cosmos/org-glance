Feature: Capture
  In order to manage things
  As a user
  I want to capture things, classes and domains

  Scenario: Let's start from scratch
    When I capture thing "New article" of class "article"
    Then I should have 1 active class
    And I should have 1 thing of class "article" registered
