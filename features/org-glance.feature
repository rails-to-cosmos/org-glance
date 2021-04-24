Feature: Do Some things
  In order to register views
  As a user
  I want to define view scopes

  Scenario: Register scope
    Given empty default scope
    And org-mode file
      """
      * Holland :Country:
      * Belgium :Country:
      * Georgia :Country:
      * Ukraine :Country:
      """
    Then I add the file to scope
    Then I should have 1 file in scope
    Then I define view "Country" with default scope
    And I should see message
      """
      View "Country" of default type is now ready to glance default scope
      """
    # Then I update view "Country"
    # And I should have 4 headlines in view "Country"
