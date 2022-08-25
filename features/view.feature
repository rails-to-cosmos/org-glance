Feature: Materialization
  Scenario: Basic Materialization
    Given store "Phones" in directory "store/phones" with headlines
      """
      * iPhone 3 :Apple:
      * iPhone 7 :Apple:
      * Йотафон :Trash:
      """

    When I create view "Apple Phones" from "Apple" "Phones"
    And I materialize view "Apple Phones" to "views/apple.org"
    And I find file "views/apple.org"
    And I go to headline with title "iPhone 3"
    And I set title of the headline at point to "iPhone 4"
    And I commit changes
    And I save buffer

    Then store "Phones" should contain headline with title "iPhone 4" in committed layer
    And store "Phones" should not contain headline with title "iPhone 4" in staging layer
    And store "Phones" should not contain headline with title "iPhone 3" in staging layer
    # And store "Phones" should not contain headline with title "iPhone 3" in committed layer
    And store "Phones" should not contain headline with title "Йотафон" in staging layer
    # And store "Phones" should not contain headline with title "Йотафон" in committed layer

    And store "Phones" should be equal to buffer store
    And view "Apple Phones" should be equal to buffer view

    When I kill current buffer
    And I find file "views/apple.org"
    Then store "Phones" should be equal to buffer store
    And view "Apple Phones" should be equal to buffer view

    # TODO: Think about when to delete headlines. For now we will preserve all headlines in committed layer
    # And store "New phones" should not contain headline with title "iPhone 3" in committed layer
    # And store "Old phones" should contain headline with title "iPhone 3" in staging layer

  Scenario: Consistent Editing
    Given store "Pets" in directory "store/pets" with headlines
      """
      * Yummi :Pomeranian:
      * Eric :Pomeranian:
      * Tanik :Human:
      """

    When I create view "Pomeranians" from "Pomeranian" "Pets"
    And I materialize view "Pomeranians" to "views/pomeranians.org"
    And I find file "views/pomeranians.org"
    And I go to headline with title "Yummi"
    And I insert " the cat"

    Then marker at point should be changed
    And marker at point should not be committed
    And 1 marker should be changed

    When I set title of the headline at point to "Yummi"

    Then marker at point should not be changed
    And 0 markers should be changed

    When I go to headline with title "Eric"
    And I insert " the dog"

    Then marker at point should be changed
    And marker at point should not be committed

    When I commit changes

    Then marker at point should be committed
    And marker at point should not be changed
    And 0 markers should be changed

    When I create view "Human Beings" from "Human" "Pets"
    And I materialize view "Human Beings" to "views/humans.org"
    And I append to file "views/humans.org"
      """
      * Some external corruption
      """
    And I find file "views/humans.org"
    And I go to headline with title "Tanik"

    Then marker at point should not be corrupted

    When I go to headline with title "Some external corruption"

    Then marker at point should be corrupted

    When I insert ": let's change this"

    Then marker at point should be corrupted

    When I insert " :thing:"

    Then marker at point should be corrupted

  # Scenario: Consistent Editing of Multiple Views
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

  # Scenario: Test visual representation of markers
  # Scenario: Remove headline
  # Scenario: Add new headline
  #   Given store "Pets" in directory "store/pets" with headlines
  #     """
  #     * Yummi :Pomeranian:
  #     * Eric :Pomeranian:
  #     * Tanik :Human:
  #     """
  #   When I materialize store "Old phones" to file "views/material.org"

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