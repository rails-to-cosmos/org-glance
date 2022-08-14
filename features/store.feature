Feature: Store
  Scenario: Import Store
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

  # Scenario: Store from Scratch
  #   Given store "Songs" in directory "songs" with headlines
  #     """
  #     * Tae Zori
  #     * Al Sok
  #     """
  #   Then store "Songs" should contain 2 staged changes
  #   And store "Songs" should contain 0 committed changes

  @dev
  Scenario: It's All About Titles
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
    And store "Stories" should contain headline with title "Travel to Romania" in staging layer
    And store "Stories" should not contain headline with title "Travel to Romania" in committed layer
    # And store "Stories" should contain headline with title "Travel to Romania (2)" in staging layer
    # And store "Stories" should not contain headline with title "Travel to Romania (2)" in committed layer

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

  @dev
  Scenario: Filters
    Given store "Pets" in directory "store/pets" with headlines
      """
      * Yummi :Pomeranian:
      * Eric :Pomeranian:
      * Tanik :Human:
      """
    When I filter headlines of class "Pomeranian" of store "Pets" to store "Pomeranians"
    Then store "Pomeranians" should contain headline with title "Yummi" in staging layer
    Then store "Pomeranians" should not contain headline with title "Yummi" in committed layer
    And store "Pomeranians" should contain headline with title "Eric" in staging layer
    And store "Pomeranians" should not contain headline with title "Eric" in committed layer
    And store "Pomeranians" should not contain headline with title "Tanik" in committed layer
    And store "Pomeranians" should not contain headline with title "Tanik" in staging layer
    And store "Pomeranians" should contain 2 headlines of class "Pomeranian"
    And store "Pomeranians" should contain 0 headlines of class "Human"

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
