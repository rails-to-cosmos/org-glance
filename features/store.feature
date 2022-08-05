Feature: Store
  Scenario: Import
    Given store "Tasks" in directory "store/tasks"
    Given file "household.org" in directory "tasks/home"
      """
      * TODO Buy milk :Task:
      * TODO Buy eggs :Task:
      """
    Given file "management.org" in directory "tasks/office"
      """
      * TODO Read mail :Task:
      * TODO Procrastinate :Task:
      """
    Given file "tasks.org.~undo-tree~" in directory "tasks/office"
      """
      * Messy stuff of undo-tree, we should ignore it
      """
    Then store "Tasks" should contain 0 headlines

    When I import headlines to store "Tasks" from directory "tasks"
    Then store "Tasks" should contain 4 headlines

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
    And store "Stories" should contain headline with title "Travel to Romania" in memory store
    And store "Stories" should contain headline with title "Travel to Romania" in persistent store

    And store "Stories" should contain headline with title "Travel to Romania (2)" in memory store
    And store "Stories" should not contain headline with title "Travel to Romania (2)" in persistent store

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
