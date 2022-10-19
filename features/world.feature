Feature: World
  Scenario: World structure
    Given world "Fantasy" in directory "fantasy"
    When I alter world "Fantasy" add dimension "State" partition by "state"
    And I alter world "Fantasy" add dimension "Tags" partition by "tag"
    And I alter world "Fantasy" add dimension "Tags" partition by "tag"
    And I add headlines to world "Fantasy"
      """
      * TODO Fish :fairytale:
      * DONE Ivan :fairytale:
      """

    Then world "Fantasy" should contain 2 dimensions
    And world "Fantasy" should contain dimension "State"
    And world "Fantasy" should contain dimension "Tags"
    # And dimension "State" of the world "Fantasy" should contain 2 views
    # And dimension "State" of the world "Fantasy" should contain view "TODO"
    # And dimension "State" of the world "Fantasy" should contain view "DONE"

  Scenario: Import from org-mode files
    Given world "Tasks" in directory "world/tasks"
    And file "tasks/home/household.org"
      """
      Some contents before the first headline.

      * TODO Buy milk :Task:
      * TODO Buy eggs :Task:
      """
    And file "tasks/office/management.org"
      """
      Some contents before the first headline.

      * TODO Read mail :Task:
      * TODO Procrastinate :Task:
      """
    And file "tasks/office/tasks.org.~undo-tree~"
      """
      * Messy stuff of undo-tree, we should ignore it
      """
    Then world "Tasks" should contain 0 headlines

    When I import headlines to world "Tasks" from directory "tasks"
    Then world "Tasks" should contain 4 headlines

  Scenario: Create world from scratch
    Given world "Songs" in directory "songs" with headlines
      """
      * Tae Zori
      * Al Sok
      """
    Then 0 staged changes should be in world "Songs"
    And 2 committed changes should be in world "Songs"
    When I persist world "Songs"
    Then 0 staged changes should be in world "Songs"
    And 2 committed changes should be in world "Songs"

  Scenario: It's all about titles
    Given world "Stories" in directory "world/stories" with headlines
      """
      * Hiking in Troodos
      * Travel to Romania
        SCHEDULED: <2022-01-01 Sat>
      * Honeymoon in Dagestan
      * Travel to Romania
        CLOSED: [2021-01-10 Sun 00:00] SCHEDULED: <2021-01-01 Fri>
      """

    Then world "Stories" should contain 4 headlines
    And world "Stories" should contain headline "Travel to Romania" in committed layer
    And world "Stories" should not contain headline "Travel to Romania" in staging layer
    # TODO Test title uniqueness
    # And world "Stories" should contain headline "Travel to Romania (2)" in staging layer
    # And world "Stories" should not contain headline "Travel to Romania (2)" in committed layer

  Scenario: Predicates
    Given world "Stories" in directory "world/stories" with headlines
      """
      * TODO COMMENT Hiking in Troodos :Travel:
      aes-encrypted V 1.3-OCB-B-4-4-M
      1/tktn7J+sRqmM2KLefQQZtIYV/FAOcDn+Rs/s5Nm17pNMFtusnXrgrjwzxWFk8F4YSBdCbbRwzl
      wUVErGnLFnK5LJ17kYnL18iRTAGhEhUQqyxXqB3DQ/41

      * TODO Travel to Romania :ARCHIVE:
        SCHEDULED: <2022-01-01 Sat>

      * TODO Secret honeymoon in Dagestan :Family:
        [[http:fsf.com]]

        + Items: Backpack

      * DONE Travel to Romania :Travel:
        CLOSED: [2021-01-10 Sun 00:00] SCHEDULED: <2021-01-01 Fri>

      """

    Then world "Stories" should contain 4 headlines
    And world "Stories" should contain 3 "TODO" headlines
    And world "Stories" should contain 1 "DONE" headline
    And world "Stories" should contain 2 headlines of class "Travel"
    And world "Stories" should contain 1 headline of class "Family"
    And world "Stories" should contain 1 archived headline
    And world "Stories" should contain 1 commented headline
    And world "Stories" should contain 1 closed headline
    And world "Stories" should contain 1 linked headline
    And world "Stories" should contain 1 propertized headline
    And world "Stories" should contain 1 encrypted headline

  # TODO conflicting mews: same location but different predicates

  # @debug
  # Scenario: Views
  #   Given world "Pets" in directory "world/pets" with headlines
  #     """
  #     * Yummi :Pomeranian:
  #     * Eric :Pomeranian:
  #     * Tanik :Human:
  #     """

  #   When I create view "Pomeranians" from "Pomeranian" "Pets" worldd in "views/pomeranians.org"
  #   And I create view "Humans" from "Human" "Pets" worldd in "views/humans.org"

    # Then world "Pets" should contain headline "Yummi" in staging layer
    # And world "Pets" should not contain headline "Yummi" in committed layer
    # And world "Pets" should contain headline "Eric" in staging layer
    # And world "Pets" should not contain headline "Eric" in committed layer
    # And world "Pets" should not contain headline "Tanik" in committed layer
    # And world "Pets" should not contain headline "Tanik" in staging layer
    # And world "Pets" should contain 2 headlines of class "Pomeranian"
    # And world "Pets" should contain 0 headlines of class "Human"

  # Scenario: Filter queries
  #   Given world "Adventures" in directory "stories/adventures" with headlines
  #     """
  #     * TODO Niagara Waterfalls :Hike:
  #     * STARTED Troodos Mountains :Hike:
  #     * STARTED Music Festival :Hike:Music:
  #     * DONE Tame Impala Concert :Music:
  #     * DONE Kamchatka :Hike:
  #     * CANCELLED PHP Course :Cringe:
  #     """

  #   And ":Hike:" "Adventures" as "Hikes"
  #   And "STARTED" "Adventures" as "Active"
  #   And ":Cringe: AND (DONE OR CANCELLED)" "Adventures" as "Archive"
  #   And ":Hike: OR :Music:" "Adventures" as "Hobby"
  #   And ":Hike: AND :Music:" "Adventures" as "Fun"
  #   And ":Hike: AND DONE" "Adventures" as "Memories"

  #   Then world "Hikes" should contain 3 headlines
  #   And world "Hikes" should contain 3 headlines of class "Hike"
