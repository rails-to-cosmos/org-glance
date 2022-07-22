Feature: Store
  Scenario: Import
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
    Given store "Tasks" in directory "tasks"
    Then store "Tasks" should contain 4 headlines

  # Scenario: Export
  #   Given store "Pets"
  #   Given file "pets.org" in directory "tasks/home"
  #     """
  #     * TODO Buy pet food :Task:
  #     * TODO Vet :Task:
  #     """
  #   When I import store "Pets" from directory "tasks"
  #   Then store "Pets" should contain 2 headlines
  #   When I export store "Pets" to directory "export"
  #   And I import store "Household" from directory "export"
  #   Then store "Pets" should be equal to "Household"

  Scenario: Materialize
    Given file "phones/original.org"
      """
      Some contents before the first headline.

      * iPhone 3 :phone:
      * Тест :phone:
      """
    And empty file "output/material.org"
    And store "Phones" in directory "phones"

    When I materialize store "Phones" to file "output/material.org"
    And I find file "output/material.org"
    And I go to the first headline
    And I set title of the headline at point to "iPhone 4"
    And I save buffer
    And I find file "phones/original.org"
    Then buffer string should be
      """
      Some contents before the first headline.

      * iPhone 4 :phone:
      * Тест :phone:
      """

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
