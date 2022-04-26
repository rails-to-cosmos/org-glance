Feature: Materialization
  Scenario: Materialize single headline
    Given headline "iphone"
      """
      * iPhone :phone:
      """
    Given registry "gadgets"
    When I materialize headline "iphone" in "gadgets"
    Then I should be in the materialized buffer
    Then I set title of the headline at point to "Samsung"
    And I apply changes
    Then I should have 1 headline in "gadgets"
    And headline title should be "Samsung"
    And headline "iphone" title should be "iPhone"

  Scenario: Materialize multiple headlines
  Scenario: Materialize encrypted headline
  Scenario: Materialize multiple headlines and add a new one
