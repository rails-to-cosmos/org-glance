Feature: Consistent Edit
  Scenario: Basic functionality
    Given world "Phones" in directory "world/phones" with headlines
      """
      * iPhone 3 :Apple:
      * iPhone 7 :Apple:
      * Йотафон :Trash:
      """

    When I create view "Apple Phones" from "(member 'apple tag)" "Phones" in "views/apple.org"
    And I find file "views/apple.org"
    And I go to headline "iPhone 3"
    And I set title of headline at point to "iPhone 4"

    Then 1 marker should be changed

    When I commit changes

    Then world "Phones" should contain headline "iPhone 4" in committed layer
    And world "Phones" should not contain headline "iPhone 4" in staging layer
    And world "Phones" should not contain headline "iPhone 3" in staging layer
    # And world "Phones" should not contain headline "iPhone 3" in committed layer
    And world "Phones" should not contain headline "Йотафон" in staging layer
    # And world "Phones" should not contain headline "Йотафон" in committed layer

    And world "Phones" should be equal to buffer world
    And view "Apple Phones" should be equal to buffer view

    When I kill current buffer
    And I find file "views/apple.org"
    Then world "Phones" should be equal to buffer world
    And view "Apple Phones" should be equal to buffer view

    # TODO: Think about when to delete headlines. For now we will preserve all headlines in committed layer
    # And world "New phones" should not contain headline "iPhone 3" in committed layer
    # And world "Old phones" should contain headline "iPhone 3" in staging layer

  Scenario: Simple changes
    Given world "Pets" in directory "world/pets" with headlines
      """
      * Yummi :Pomeranian:
        Black and White Doggy
      * Eric :Pomeranian:
      * Tanik :Human:
      """

    When I create view "Pomeranians" from "(member 'pomeranian tag)" "Pets" in "views/pomeranians.org"

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

    When I commit changes

    And marker at point should not be changed
    And 0 markers should be changed

    When I create view "Human Beings" from "(member 'human tag)" "Pets" in "views/humans.org"
    And kill buffer
    And I append to file "views/humans.org"
      """
      * Some external corruption
      """
    And I find file "views/humans.org"
    And I go to headline "Tanik"

  Scenario: Change headline todo state
    Given world "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      * TODO Samovar :Friends:
      """

    When I create view "Pigs" from "(member 'peppa tag)" "Wishlist" in "views/pigs.org" as "*pigs*"
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
    Given world "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      * TODO Samovar :Friends:
      """

    When I create view "Pigs" from "(member 'peppa tag)" "Wishlist" in "views/pigs.org"
    And go to headline "Peppa Pig"

    Then marker at point should not be changed
    And 0 markers should be changed

    When I set headline tags to ":Beacon:"
    Then marker at point should be changed
    And 1 marker should be changed
    And markers positions should be consistent

  Scenario: Change headline title without changing todo state
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      """
    When I create view "Hikes" from "(member 'hike tag)" "Adventures" in "views/hikes.org" as "*hikes*"

    Then current buffer should contain 2 headlines
    And buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "Niagara Waterfalls"
    And set title of headline at point to "Niagara Waterfalls 2020"

    Then markers positions should be consistent

  Scenario: Change headline title with changing todo state (STARTED state is not registered)
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      """
    When I create view "Hikes" from "(member 'hike tag)" "Adventures" in "views/hikes.org" as "*hikes*"

    Then current buffer should contain 2 headlines
    And buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "STARTED Troodos Mountains"
    And set title of headline at point to "STARTED Troodos Mountains 2019"

    Then markers positions should be consistent

  Scenario: Change headline contents
    Given world "Wishlist" in directory "nerdy" with headlines
      """
      * TODO Tatinek :Peppa:
      * TODO Peppa Pig :Peppa:
      """

    When I create view "Pigs" from "(member 'peppa tag)" "Wishlist" in "views/pigs.org" as "*pigs*"
    And set headline "Tatinek" contents to
    """
    SCHEDULED: <2019-08-23 Fri>
    """
    And save buffer

    Then marker positions and hashes should be consistent

  Scenario: Sync commented headlines
    Given world "Wishlist" in directory "nerdy" with headlines
      """
      * TODO COMMENT Tatinek :Peppa:Gift:
      """

    When I create view "Pigs" from "(member 'peppa tag)" "Wishlist" in "views/pigs.org" as "*pigs*"
    And create view "Gifts" from "(member 'gift tag)" "Wishlist" in "views/gifts.org" as "*gifts*"
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
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "(member 'hike tag)" "Adventures" in "views/hikes.org" as "*hikes*"
    And create view "Fun" from "(member 'music tag)" "Adventures" in "views/fun.org" as "*fun*"
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
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "(member 'hike tag)" "Adventures" in "views/hikes.org" as "*hikes*"
    And create view "Fun" from "(member 'music tag)" "Adventures" in "views/fun.org"
    And kill buffer
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
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
        SCHEDULED: <2022-09-16 Fri>
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I create view "Hikes" from "(and (member 'hike tag) (member 'todo state))" "Adventures" in "views/hikes.org" as "*hikes*"
    And create view "Fun" from "(member 'music tag)" "Adventures" in "views/fun.org" as "*fun*"
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

    # Then headline "Music Festival 2022" should be changed

    When I rename headline "Music Festival 2022" to "Music Festival 2023"
    And set headline "Music Festival 2023" contents to
    """
    SCHEDULED: <2023-01-01 Sun>
    """

    Then headline "Music Festival 2023" should be changed
    And 1 marker should be changed

    When I commit changes

    Then marker positions and hashes should be consistent

    When I switch to buffer "*hikes*"

    Then I should be in buffer "*hikes*"
    And current buffer should contain 2 headlines
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
    Given world "Adventures" in directory "stories/adventures" with headlines
      """
      * TODO Niagara Waterfalls :Hike:
      * STARTED Troodos Mountains :Hike:
      * TODO Music Festival :Hike:Music:
        SCHEDULED: <2022-09-16 Fri>
      * DONE Tame Impala Concert :Music:
      * DONE Kamchatka :Hike:
      * CANCELLED PHP Course :Cringe:
      """

    When I select view from "Adventures" where tag = "Hike"
    And rename headline "Music Festival" to "Music Festival 2022"
    And set headline "Music Festival 2022" contents to
    """
    SCHEDULED: <2022-01-01 Sat>
    """

    Then headline "Music Festival" should not be in current buffer
    And headline "Music Festival 2022" should be in current buffer
    And buffer offset should be latest

    When I save buffer

    Then headline "Music Festival" should not be in current buffer
    And headline "Music Festival 2022" should be in current buffer

    When I select view from "Adventures" where tag = "Music"

    Then current buffer should contain 2 headlines
    And marker positions should be consistent
    And marker positions and hashes should be consistent
    And headline "Music Festival" should not be in current buffer
    And headline "Music Festival 2022" should be in current buffer

    And the contents of headline "Music Festival 2022" should be
    """
    SCHEDULED: <2022-01-01 Sat>
    """
    And buffer offset should be latest

    When I rename headline "Music Festival 2022" to "Music Festival 2023"
    And set headline "Music Festival 2023" contents to
    """
    SCHEDULED: <2023-01-01 Sun>
    """

    Then headline "Music Festival 2023" should be changed
    And 1 marker should be changed

    When I save buffer

    Then marker positions and hashes should be consistent
    And buffer offset should be latest

    When I kill buffer
    And I select view from "Adventures" where tag = "Hike"

    Then current buffer should contain 4 headlines
    And headline "Music Festival 2022" should not be in current buffer
    And headline "Music Festival 2023" should be in current buffer
    And the contents of headline "Music Festival 2023" should be
    """
    SCHEDULED: <2023-01-01 Sun>
    """
    And marker positions and hashes should be consistent
    And buffer offset should be latest

    When I rename headline "Music Festival 2023" to "Music Festival 2024"
    And save buffer
    And kill buffer
    And I select view from "Adventures" where tag = "Music"

    Then current buffer should contain 2 headlines
    And marker positions and hashes should be consistent
    And headline "Music Festival 2023" should not be in current buffer
    And headline "Music Festival 2024" should be in current buffer
    And buffer offset should be latest

    # When I commit changes
    # And kill buffer "*fun*"
    # And append to file "views/fun.org"
    #   """
    #   * Some external corruption
    #   """
    # And find file "views/fun.org"

    # And I create view "Active" from "STARTED" "Adventures"
    # And I create view "Archive" from "DONE OR CANCELLED" "Adventures"
    # And I create world "Hobby" from "Hike OR Music" "Adventures"

    # And I create world "Fun" from ":Hike: AND :Music:" "Adventures"
    # And I create world "Memories" from ":Hike: AND DONE" "Adventures"

  Scenario: Headline updates without accessing previous state
    Given world "Adventures"

    # When I alter world "Adventures" add dimension "State" partition by "state"
    And add headlines to world "Adventures"
      """
      * TODO Niagara Waterfalls
      """

    # Then world "Adventures" should contain 1 dimension
    # And world "Adventures" should contain dimension "State"
    And world "Adventures" should contain view "TODO" derived from dimension "State"

    When I visit view "TODO" derived from dimension "State" in world "Adventures"

    Then current buffer should contain 1 headline
    And buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "Niagara Waterfalls"
    And set headline todo state to "DONE"

    Then marker at point should be changed
    And markers positions should be consistent

    When I commit changes
    And kill buffer

    Then world "Adventures" should contain view "DONE" derived from dimension "State"

    When I visit view "DONE" derived from dimension "State" in world "Adventures"

    Then current buffer should contain 1 headline
    And buffer offset should be latest
    And marker positions and hashes should be consistent

    When I go to headline "Niagara Waterfalls"
    And set headline todo state to "TODO"

    Then marker at point should be changed
    And markers positions should be consistent

    When I commit changes
    And kill buffer
    And visit view "TODO" derived from dimension "State" in world "Adventures"

    Then current buffer should contain 1 headline

    When I visit view "DONE" derived from dimension "State" in world "Adventures"

    Then current buffer should contain 0 headlines

  # TODO Sync COMMENTED property
  # TODO Sync buffers update offset (maybe not)
  # TODO Test external corruptions
  # TODO do not update headline if hashes are equal
  # Scenario: Test visual representation of markers
  # Scenario: Remove headline
  # Scenario: Add new headline
  #   Given world "Pets" in directory "world/pets" with headlines
  #     """
  #     * Yummi :Pomeranian:
  #     * Eric :Pomeranian:
  #     * Tanik :Human:
  #     """
  #   When I materialize world "Old phones" to file "views/material.org"

# TODO:
# Scenario: Delete headline
# Scenario: Merge conflict: same headline changed in separate buffer and committed
# Scenario: Edit world from two buffers, fast-forward change should be reflected in other buffer
# Scenario: Material fast-forward commit: write outdated mew commit scenarios
# Scenario: Material offset should be set to actual state after commit
# Scenario: garbage collector. Maybe we shouldn't remove headlines from persistent storage on commit?

#   Scenario: Materialize encrypted headline
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
#   Optimize mew: mark headlines as changed and apply only them
