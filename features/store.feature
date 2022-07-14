Feature: Store
  Scenario: Import
    Given store "Tasks"
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
    When I import store "Tasks" from directory "tasks"
    Then store "Tasks" should contain 4 headlines

  Scenario: Export
    Given store "Pets"
    Given file "pets.org" in directory "tasks/home"
      """
      * TODO Buy pet food :Task:
      * TODO Vet :Task:
      """
    When I import store "Pets" from directory "tasks"
    Then store "Pets" should contain 2 headlines
    When I export store "Pets" to directory "export"
    And I import store "Household" from directory "export"
    Then store "Pets" should be equal to "Household"

  Scenario: Materialize
    Given store "Phones"
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

  Scenario: Materialize headlines with unicode titles
    Given store "Cyrillic Notes"
    Given file "origin.org"
      """
      * Тест :note:
      """
    Given empty file "material.org"
    When I import store "Cyrillic Notes" from file "origin.org"
    And I materialize store "Cyrillic Notes" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    And I set title of the headline at point to "Тест 2"
    And I save buffer
    And I find file "origin.org"
    Then buffer string should be
      """
      * Тест 2 :note:
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
  Scenario: Relative path in materialization
