Feature: Do Some things
  In order to do something
  As a user
  I want to do something

  Scenario: Do Something
    Given I have "something"
    When I have "something"
    Then I should have "something"
    And I should have "something"
    But I should not have "something"

Feature: Define Views
  In order to perform actions on views
  As a user
  I want to define org-glance views

  Background:
    Given
    And the buffer is empty
    And I insert
      """
      * Netherlands :Country:
      * Belgium :Country:
      * Georgia :Country:
      """

  Scenario: Visit Headline
