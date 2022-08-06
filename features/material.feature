Feature: Materialization
  Scenario: Basic materialization
    Given store "Old phones" in directory "store/phones" with headlines
      """
      * iPhone 3 :phone:
      * Тест :phone:
      """
    Given empty file "views/material.org"

    When I materialize store "Old phones" to file "views/material.org"
    And I find file "views/material.org"
    And I go to headline with title "iPhone 3"
    And I set title of the headline at point to "iPhone 4"
    And I commit changes to store "New phones"


    Then store "New phones" should contain headline with title "iPhone 4" in memory store
    And store "New phones" should contain headline with title "iPhone 4" in persistent store
    And store "New phones" should not contain headline with title "iPhone 3" in memory store

    # Think when to delete headlines. For now we will preserve all headlines in persistent store
    # And store "New phones" should not contain headline with title "iPhone 3" in persistent store
    And store "Old phones" should contain headline with title "iPhone 3" in memory store
    And store "Old phones" should contain headline with title "iPhone 3" in persistent store

# TODO:
# Scenario: Merge conflict: same headline changed in separate buffer and committed
# Scenario: Edit store from two buffers, fast-forward change should be reflected in other buffer
# Scenario: Material fast-forward commit: write outdated materialization commit scenarios
# Scenario: Material offset should be set to actual state after commit
# Scenario: garbage collector. Maybe we shouldn't remove headlines from persistent storage on commit?

#   Scenario: Materialize encrypted headline
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
#   Optimize materialization: mark headlines as changed and apply only them
