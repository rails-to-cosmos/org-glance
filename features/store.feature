Feature: Store
  Scenario: Import from org-mode files
    Given store "Tasks" in directory "store/tasks"
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
    Then store "Tasks" should contain 0 headlines

    When I import headlines to store "Tasks" from directory "tasks"
    Then store "Tasks" should contain 4 headlines

  Scenario: Create from scratch
    Given store "Songs" in directory "songs" with headlines
      """
      * Tae Zori
      * Al Sok
      """
    Then 0 staged changes should be in store "Songs"
    And 2 committed changes should be in store "Songs"
    When I flush store "Songs"
    Then 0 staged changes should be in store "Songs"
    And 2 committed changes should be in store "Songs"

  Scenario: It's all about titles
    Given store "Stories" in directory "store/stories" with headlines
      """
      * Hiking in Troodos
      * Travel to Romania
        SCHEDULED: <2022-01-01 Sat>
      * Honeymoon in Dagestan
      * Travel to Romania
        CLOSED: [2021-01-10 Sun 00:00] SCHEDULED: <2021-01-01 Fri>
      """

    Then store "Stories" should contain 4 headlines
    And store "Stories" should contain headline "Travel to Romania" in committed layer
    And store "Stories" should not contain headline "Travel to Romania" in staging layer
    # TODO Test title uniqueness
    # And store "Stories" should contain headline "Travel to Romania (2)" in staging layer
    # And store "Stories" should not contain headline "Travel to Romania (2)" in committed layer

  Scenario: Predicates
    Given store "Stories" in directory "store/stories" with headlines
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

    Then store "Stories" should contain 4 headlines
    And store "Stories" should contain 3 "TODO" headlines
    And store "Stories" should contain 1 "DONE" headline
    And store "Stories" should contain 2 headlines of class "Travel"
    And store "Stories" should contain 1 headline of class "Family"
    And store "Stories" should contain 1 archived headline
    And store "Stories" should contain 1 commented headline
    And store "Stories" should contain 1 closed headline
    And store "Stories" should contain 1 linked headline
    And store "Stories" should contain 1 propertized headline
    And store "Stories" should contain 1 encrypted headline

  # TODO conflicting mews: same location but different predicates

  Scenario: Views
    Given store "Pets" in directory "store/pets" with headlines
      """
      * Yummi :Pomeranian:
      * Eric :Pomeranian:
      * Tanik :Human:
      """

    When I create view "Pomeranians" from "Pomeranian" "Pets"
    And I materialize view "Pomeranians" to "views/pomeranians.org"
    And I create view "Humans" from "Human" "Pets"
    And I materialize view "Humans" to "views/humans.org"

    # Then store "Pets" should contain headline "Yummi" in staging layer
    # And store "Pets" should not contain headline "Yummi" in committed layer
    # And store "Pets" should contain headline "Eric" in staging layer
    # And store "Pets" should not contain headline "Eric" in committed layer
    # And store "Pets" should not contain headline "Tanik" in committed layer
    # And store "Pets" should not contain headline "Tanik" in staging layer
    # And store "Pets" should contain 2 headlines of class "Pomeranian"
    # And store "Pets" should contain 0 headlines of class "Human"

  # Scenario: Filter queries
  #   Given store "Adventures" in directory "stories/adventures" with headlines
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

  #   Then store "Hikes" should contain 3 headlines
  #   And store "Hikes" should contain 3 headlines of class "Hike"
