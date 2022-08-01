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
    And I select headline with title "iPhone 3"
    And I set title of the headline at point to "iPhone 4"
    And I commit changes to store "Phones"

    Then store "Phones" should contain headline with title "iPhone 4" in memory store
    Then store "Phones" should contain headline with title "iPhone 4" in persistent store

    And store "Phones" should not contain headline with title "iPhone 3" in memory store
    And store "Phones" should not contain headline with title "iPhone 3" in persistent store

# TODO:
# Scenario: Merge conflict: same headline changed in separate buffer and committed
# Scenario: Edit store from two buffers, fast-forward change should be reflected in other buffer
# Scenario: Material fast-forward commit: write outdated materialization commit scenarios
# Scenario: Material offset should be set to actual state after commit
# Scenario: garbage collector. Maybe we shouldn't remove headlines from persistent storage on commit?
