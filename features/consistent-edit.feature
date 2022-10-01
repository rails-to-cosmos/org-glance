Feature: Consistent Edit
  Scenario: Basic functionality
    Given store "Phones" in directory "store/phones" with headlines
      """
      * iPhone 3 :Apple:
      * iPhone 7 :Apple:
      * Йотафон :Trash:
      """

    When I create view "Apple Phones" from "Apple" "Phones"
    And I materialize view "Apple Phones" to "views/apple.org"
    And I find file "views/apple.org"
    And I go to headline "iPhone 3"
    And I set title of headline at point to "iPhone 4"

    Then 1 marker should be changed

    When I commit changes

    Then store "Phones" should contain headline "iPhone 4" in committed layer
    And store "Phones" should not contain headline "iPhone 4" in staging layer
    And store "Phones" should not contain headline "iPhone 3" in staging layer
    # And store "Phones" should not contain headline "iPhone 3" in committed layer
    And store "Phones" should not contain headline "Йотафон" in staging layer
    # And store "Phones" should not contain headline "Йотафон" in committed layer

    And store "Phones" should be equal to buffer store
    And view "Apple Phones" should be equal to buffer view

    When I kill current buffer
    And I find file "views/apple.org"
    Then store "Phones" should be equal to buffer store
    And view "Apple Phones" should be equal to buffer view

    # TODO: Think about when to delete headlines. For now we will preserve all headlines in committed layer
    # And store "New phones" should not contain headline "iPhone 3" in committed layer
    # And store "Old phones" should contain headline "iPhone 3" in staging layer

  Scenario: Simple changes
    Given store "Pets" in directory "store/pets" with headlines
      """
      * Yummi :Pomeranian:
        Black and White Doggy
      * Eric :Pomeranian:
      * Tanik :Human:
      """

    When I create view "Pomeranians" from "Pomeranian" "Pets"
    And I materialize view "Pomeranians" to "views/pomeranians.org"
    And I find file "views/pomeranians.org"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent

    # Edit before the first headline
    When I go to the beginning of buffer
    And I insert "#+TITLE: The Zoo\n"

    Then markers positions should be consistent

    # Deletions should be properly handled
    When I go to the beginning of buffer
    And I kill current line

    Then markers positions should be consistent

    When I go to headline "Yummi"
    And I insert " the cat"

    Then marker at point should be changed
    And marker at point should not be committed
    And 1 marker should be changed
    And markers positions should be consistent

    # Remove *return-to-unchanged-state* feature to optimize after-change-functions
    # When I set title of headline at point to "Yummi"

    # Then marker at point should not be changed
    # And 0 markers should be changed
    # And marker positions and hashes should be consistent

    When I go to headline "Eric"
    And I insert " the dog"

    Then marker at point should be changed
    And 2 markers should be changed
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
    And I go to headline "Tanik"

    Then marker at point should not be corrupted

    When I go to headline "Some external corruption"

    Then marker at point should be corrupted

    When I insert ": let's change this"

    Then marker at point should be corrupted

    When I insert " :thing:"

    Then marker at point should be corrupted

  Scenario: Change headline todo state
    Given store "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      * TODO Samovar :Friends:
      """

    When I create view "Pigs" from "Peppa" "Wishlist" in file "views/pigs.org" as "*pigs*"
    And switch to buffer "*pigs*"
    And go to headline "Peppa Pig"

    Then marker at point should not be changed
    And 0 markers should be changed

    When I set headline todo state to "DONE"
    Then marker at point should be changed
    # And 1 marker should be changed
    And markers positions should be consistent

    When I commit changes

    Then marker positions and hashes should be consistent

  Scenario: Change headline tags
    Given store "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      * TODO Samovar :Friends:
      """

    When I create view "Pigs" from "Peppa" "Wishlist"
    And materialize view "Pigs" to "views/pigs.org"
    And find file "views/pigs.org"
    And go to headline "Peppa Pig"

    Then marker at point should not be changed
    And 0 markers should be changed

    When I set headline tags to ":Beacon:"
    Then marker at point should be changed
    And 1 marker should be changed
    And markers positions should be consistent

  Scenario: Change headline title without changing todo state
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      """
    When I create view "Hikes" from "Hike" "Adventures"
    And materialize view "Hikes" to "views/hikes.org"
    And find file "views/hikes.org" as "*hikes*"

    Then current buffer should contain 2 headlines
    And current buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "Niagara Waterfalls"
    And set title of headline at point to "Niagara Waterfalls 2020"

    Then markers positions should be consistent

  Scenario: Change headline title with changing todo state (STARTED state is not registered)
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      """
    When I create view "Hikes" from "Hike" "Adventures"
    And materialize view "Hikes" to "views/hikes.org"
    And find file "views/hikes.org" as "*hikes*"

    Then current buffer should contain 2 headlines
    And current buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "STARTED Troodos Mountains"
    And set title of headline at point to "STARTED Troodos Mountains 2019"

    Then markers positions should be consistent

  Scenario: Change headline contents
    Given store "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      """

    When I create view "Pigs" from "Peppa" "Wishlist" in file "views/pigs.org" as "*pigs*"
    And switch to buffer "*pigs*"
    And set headline "Tatinek" contents to
    """
    SCHEDULED: <2019-08-23 Fri>
    """
    And save buffer

    Then marker positions and hashes should be consistent

  Scenario: Sync commented headlines
    Given store "Wishlist" in directory "nerdy" with headlines
      """
      * TODO COMMENT Tatinek :Peppa:Gift:
      """

    When I create view "Pigs" from "Peppa" "Wishlist" in file "views/pigs.org" as "*pigs*"
    And create view "Gifts" from "Gift" "Wishlist" in file "views/gifts.org" as "*gifts*"
    And switch to buffer "*pigs*"
    And set headline "Tatinek" contents to
    """
    SCHEDULED: <2019-08-23 Fri>
    """
    And save buffer

    Then marker positions and hashes should be consistent

    When I switch to buffer "*gifts*"

    Then headline "Tatinek" should be commented

  Scenario: Real-time sync in live buffers
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "Hike" "Adventures" in file "views/hikes.org" as "*hikes*"
    And create view "Fun" from "Music" "Adventures" in file "views/fun.org" as "*fun*"
    And I switch to buffer "*hikes*"
    And rename headline "Music Festival" to "Music Festival 2022"
    And rename headline "Kamchatka" to "Kamchatka 2019"
    And commit changes
    And switch to buffer "*fun*"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent
    And headline "Music Festival 2022" should be in current buffer
    And headline "Music Festival" should not be in current buffer

  Scenario: Sync on read
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "Hike" "Adventures" in file "views/hikes.org" as "*hikes*"
    And create view "Fun" from "Music" "Adventures"
    And materialize view "Fun" to "views/fun.org"
    And switch to buffer "*hikes*"
    And rename headline "Music Festival" to "Music Festival 2022"
    And rename headline "Kamchatka" to "Kamchatka 2019"
    And save buffer

    Then marker positions and hashes should be consistent

    When I find file "views/fun.org" as "*fun*"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent
    And headline "Music Festival 2022" should be in current buffer
    And headline "Music Festival" should not be in current buffer

  Scenario: Multiple views, modifications across buffers
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
        SCHEDULED: <2022-09-16 Fri>
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "Hike" "Adventures" in file "views/hikes.org" as "*hikes*"
    And create view "Fun" from "Music" "Adventures" in file "views/fun.org" as "*fun*"
    And switch to buffer "*hikes*"

    When I rename headline "Music Festival" to "Music Festival 2022"
    And set headline "Music Festival 2022" contents to
    """
    SCHEDULED: <2022-01-01 Sat>
    """
    And save buffer
    And switch to buffer "*fun*"

    Then current buffer should contain 2 headlines
    And marker positions should be consistent
    And marker positions and hashes should be consistent
    And headline "Music Festival" should not be in current buffer
    And headline "Music Festival 2022" should be in current buffer
    And the contents of headline "Music Festival 2022" should be
    """
    SCHEDULED: <2022-01-01 Sat>
    """

    Then headline "Music Festival 2022" should be changed

    When I rename headline "Music Festival 2022" to "Music Festival 2023"
    And set headline "Music Festival 2023" contents to
    """
    SCHEDULED: <2023-01-01 Sun>
    """

    Then headline "Music Festival 2023" should be changed
    And headline "Music Festival 2023" should not be committed
    And headline "Music Festival 2023" should not be corrupted
    And 1 marker should be changed

    When I commit changes

    Then marker positions and hashes should be consistent

    When I switch to buffer "*hikes*"

    Then I should be in buffer "*hikes*"
    And current buffer should contain 4 headlines
    And headline "Music Festival 2022" should not be in current buffer
    And headline "Music Festival 2023" should be in current buffer
    And the contents of headline "Music Festival 2023" should be
    """
    SCHEDULED: <2023-01-01 Sun>
    """
    And marker positions and hashes should be consistent

    When I rename headline "Music Festival 2023" to "Music Festival 2024"
    And save buffer
    And switch to buffer "*fun*"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent
    And headline "Music Festival 2023" should not be in current buffer
    And headline "Music Festival 2024" should be in current buffer

  @debug
  Scenario: Multiple views, modifications across files
    Given store "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
        SCHEDULED: <2022-09-16 Fri>
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "Hike" "Adventures" in file "views/hikes.org"
    And create view "Fun" from "Music" "Adventures" in file "views/fun.org"
    And find file "views/hikes.org"
    And I rename headline "Music Festival" to "Music Festival 2022"
    And set headline "Music Festival 2022" contents to
    """
    SCHEDULED: <2022-01-01 Sat>
    """
    And save buffer
    And kill buffer
    And find file "views/fun.org"

    Then current buffer should contain 2 headlines
    And marker positions should be consistent
    And marker positions and hashes should be consistent
    And headline "Music Festival" should not be in current buffer
    And headline "Music Festival 2022" should be in current buffer
    And the contents of headline "Music Festival 2022" should be
    """
    SCHEDULED: <2022-01-01 Sat>
    """
    And current buffer offset should be latest

    When I rename headline "Music Festival 2022" to "Music Festival 2023"
    And set headline "Music Festival 2023" contents to
    """
    SCHEDULED: <2023-01-01 Sun>
    """

    Then headline "Music Festival 2023" should be changed
    And headline "Music Festival 2023" should not be committed
    And headline "Music Festival 2023" should not be corrupted
    And 1 marker should be changed

    When I save buffer

    Then marker positions and hashes should be consistent
    And current buffer offset should be latest

    When I kill buffer
    And find file "views/hikes.org"

    Then current buffer should contain 4 headlines
    And headline "Music Festival 2022" should not be in current buffer
    And headline "Music Festival 2023" should be in current buffer
    And the contents of headline "Music Festival 2023" should be
    """
    SCHEDULED: <2023-01-01 Sun>
    """
    And marker positions and hashes should be consistent
    And current buffer offset should be latest

    When I rename headline "Music Festival 2023" to "Music Festival 2024"
    And save buffer
    And kill buffer
    And find file "views/fun.org"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent
    And headline "Music Festival 2023" should not be in current buffer
    And headline "Music Festival 2024" should be in current buffer
    And current buffer offset should be latest

    # When I commit changes
    # And kill buffer "*fun*"
    # And append to file "views/fun.org"
    #   """
    #   * Some external corruption
    #   """
    # And find file "views/fun.org"

    # And I create view "Active" from "STARTED" "Adventures"
    # And I create view "Archive" from "DONE OR CANCELLED" "Adventures"
    # And I create store "Hobby" from "Hike OR Music" "Adventures"

    # And I create store "Fun" from ":Hike: AND :Music:" "Adventures"
    # And I create store "Memories" from ":Hike: AND DONE" "Adventures"

  # TODO Sync COMMENTED property
  # TODO Sync buffers update offset (maybe not)
  # TODO Test external corruptions
  # TODO do not update headline if hashes are equal
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
# Scenario: Delete headline
# Scenario: Merge conflict: same headline changed in separate buffer and committed
# Scenario: Edit store from two buffers, fast-forward change should be reflected in other buffer
# Scenario: Material fast-forward commit: write outdated mew commit scenarios
# Scenario: Material offset should be set to actual state after commit
# Scenario: garbage collector. Maybe we shouldn't remove headlines from persistent storage on commit?

#   Scenario: Materialize encrypted headline
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
#   Optimize mew: mark headlines as changed and apply only them
