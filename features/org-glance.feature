Feature: Do Some things
  In order to work with views
  As a user
  I want to define views

  Scenario: Register scope and view
    Given empty default scope
    And org-mode file
      """
      * Holland :Country:
      * Belgium :Country:
      * Georgia :Country:
      * Ukraine :Country:
      """
    When I add the file to scope
    Then I should have 1 file in scope
    And I should have 0 views registered

    When I define view "Country" with default scope
    Then I should see message
      """
      View "Country" of default type is now ready to glance default scope
      """
    And I should have 4 headlines in view "Country"
    And I should have 1 view registered
    And I should have a choice of 4 headlines for "visit" action
    # When I run action "visit" for headlines and type "Bel RET"
    # Then I should see "* Holland :Country:"
