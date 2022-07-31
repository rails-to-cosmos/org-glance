Feature: Materialization and material mode
  @dev
  Scenario: Materialize store
    Given empty file "views/material.org"
    And file "notes/phones/original.org"
      """
      Some contents before the first headline.

      * iPhone 3 :phone:
      * Тест :phone:
      """
    And store "Phones" in directory "stores/phones"

    When I import headlines to store "Phones" from directory "notes/phones"
    Then store "Phones" should contain 2 headlines

    When I materialize store "Phones" to file "views/material.org"
    And I find file "views/material.org"
    And I go to headline with title "iPhone 3"
    And I set title of the headline at point to "iPhone 4"
    And I commit changes to store "Phones"

    Then store "Phones" should contain headline with title "iPhone 4"
    And store "Phones" should not contain headline with title "iPhone 3"

    # # Original file should not change
    # And I find file "notes/phones/original.org"
    # Then buffer string should be
    #   """
    #   Some contents before the first headline.

    #   * iPhone 3 :phone:
    #   * Тест :phone:
    #   """
