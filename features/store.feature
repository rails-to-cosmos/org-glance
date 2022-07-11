Feature: Store
  Scenario: Import from directory
    Given store "TaskStore"
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
    When I import store "TaskStore" from directory "tasks"
    Then store "TaskStore" should contain 4 headlines

  Scenario: Filter by class
  Scenario: Filter by user properties
  Scenario: Filter by archived property
  Scenario: Filter by commented property
  Scenario: Filter by closed property
  Scenario: Filter by encrypted property
  Scenario: Filter by linked property

  Scenario: Materialize store
    Given file "origin.org"
      """
      Some contents before the first headline.

      * iPhone 3 :phone:
      * Nokia :phone:
      """
    Given empty file "material.org"

    When I import store "Phones" from file "origin.org"
    And I materialize store "Phones" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    And I set title of the headline at point to "iPhone 4"
    And I save buffer
    And I find file "origin.org"
    Then buffer string should be
      """
      Some contents before the first headline.

      * iPhone 4 :phone:
      * Nokia :phone:
      """

#   Scenario: Create headline from materialized buffer ? we need to now where to find location headline store
#   Scenario: Materialize encrypted headline
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
