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

  @dev
  Scenario: Materialize
    Given empty file "views/material.org"
    Given file "notes/phones/original.org"
      """
      Some contents before the first headline.

      * iPhone 3 :phone:
      * Тест :phone:
      """
    Given store "Phones" in directory "stores/phones"

    When I import headlines to store "Phones" from directory "notes/phones"
    Then store "Phones" should contain 2 headlines

    When I materialize store "Phones" to file "views/material.org"
    And I find file "views/material.org"
    And I go to the first headline
    And I set title of the headline at point to "iPhone 4"
    And I save buffer
    And I commit changes to store "Phones"

    # Then store "Phones" should contain headline "iPhone 4"
    # # TODO: remove old headlines
    # And store "Phones" should not contain headline "iPhone 3"

    # # Original file should not change
    # And I find file "notes/phones/original.org"
    # Then buffer string should be
    #   """
    #   Some contents before the first headline.

    #   * iPhone 3 :phone:
    #   * Тест :phone:
    #   """

#   Scenario: Create headline from materialized buffer ? we need to now where to find location headline store
#   Scenario: Materialize encrypted headline
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
#   Optimize materialization: mark headlines as changed and apply only them

  Scenario: Filter by class
  Scenario: Filter by user properties
  Scenario: Filter by archived property
  Scenario: Filter by commented property
  Scenario: Filter by closed property
  Scenario: Filter by encrypted property
  Scenario: Filter by linked property
  Scenario: Relative paths
