Feature: Materialization
  @debug
  Scenario: Basic materialization
    Given store "Phones" in directory "store/phones" with headlines
      """
      * iPhone 3 :Apple:
      * Йотафон :Trash:
      """

    When I create view "Apple Phones" from "Apple" "Phones"
    And I materialize view "Apple Phones" to file "views/apple.org"
    And I find file "views/apple.org"
    And I go to headline with title "iPhone 3"
    And I set title of the headline at point to "iPhone 4"
    And I commit changes to store "New phones"

    Then store "New phones" should contain headline with title "iPhone 4" in committed layer
    And store "New phones" should not contain headline with title "iPhone 4" in staging layer
    And store "New phones" should not contain headline with title "iPhone 3" in staging layer
    And store "New phones" should not contain headline with title "iPhone 3" in committed layer

    # TODO: Think about when to delete headlines. For now we will preserve all headlines in committed layer
    # And store "New phones" should not contain headline with title "iPhone 3" in committed layer
    # And store "Old phones" should contain headline with title "iPhone 3" in staging layer

  # @dev
  # Scenario: Change headlines
  #   Given store "Pets" in directory "store/pets" with headlines
  #     """
  #     * Yummi :Pomeranian:
  #     * Eric :Pomeranian:
  #     * Tanik :Human:
  #     """
  #   When I materialize store "Pets" to file "views/material.org"
  #   And I go to headline with title "Yummi"
  #   And I insert " the dog"
  #   Then headline materialization at point should be changed
  #   And headline materialization at point should not be outdated

  # Scenario: Add new headline
  #   Given store "Pets" in directory "store/pets" with headlines
  #     """
  #     * Yummi :Pomeranian:
  #     * Eric :Pomeranian:
  #     * Tanik :Human:
  #     """
  #   When I materialize store "Old phones" to file "views/material.org"

  # Scenario: 1+ materializations
  #   Given store "Adventures" in directory "stories/adventures" with headlines
  #     """
  #     * TODO Niagara Waterfalls :Hike:
  #     * STARTED Troodos Mountains :Hike:
  #     * STARTED Music Festival :Hike:Music:
  #     * DONE Tame Impala Concert :Music:
  #     * DONE Kamchatka :Hike:
  #     * CANCELLED PHP Course :Cringe:
  #     """
  #   When I create store "Hikes" from ":Hike:" "Adventures"
  #   And I create store "Active" from "STARTED" "Adventures"
  #   And I create store "Archive" from "DONE OR CANCELLED" "Adventures"
  #   And I create store "Hobby" from ":Hike: OR :Music:" "Adventures"
  #   And I create store "Fun" from ":Hike: AND :Music:" "Adventures"
  #   And I create store "Memories" from ":Hike: AND DONE" "Adventures"

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
