@core
Feature: Capture subtree at point
  In order to manage current headline as a part of org-glance view
  As a user
  I want to capture org-mode headlines

  @org-mode
  Scenario: Capture subtree, specify view and working directory
    Given I'm in a user directory
    And I create file "inventory.org"
      """
      * Google Pixel 3A
      * Wahl 8164-516
      """
    And I add file "inventory.org" to default scope
    And I define view "Item" in default scope
    And I find file "inventory.org"
    And I search forward "Google Pixel"
    Then I should see "Google Pixel 3A"
    And I capture the subtree as "Item"
    Then I should be prompted to choose directory
    And I type "items RET"
    And directory "items" should appear
    And element property "DIR" should be equal to "items"
    And element tag string should be equal to ":Item:"
