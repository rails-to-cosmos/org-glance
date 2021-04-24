@core
Feature: Initialization
  In order to manage views
  As a user
  I want to define views

  Scenario: Prerequisites
    Given empty default scope
    Given I'm in a root directory

    # Implement directory structure for user data and system data
    And I create directory "org-glance-views"
    And I create directory "user-data"

    # Set org-glance-view-location
    When I change directory to "org-glance-views"
    And I set view location to current directory
    And I change directory to parent directory

  @scope
  Scenario: Default scope
    When I change directory to "user-data"
    And I create org-mode file "countries.org"
      """
      * Holland :Country:
      * Belgium :Country:
      * Georgia :Country:
      * Ukraine :Country:
      """
    When I add the file to default scope
    Then I should have 1 file in default scope
    And I change directory to parent directory

  @scope @view
  Scenario: View definition
    When I define view "Country" with default scope
    Then I should see message
      """
      View "Country" is now ready to glance
      """
    And I should have 4 headlines in view "Country"
    And I should have 1 view registered

  @view @action
  Scenario: Perform VISIT action
    When I run action "visit" for headlines and type "[Country] SPC Holland RET"
    Then I should see "Holland"

    When I run action "visit" for headlines and type "[Country] SPC Ukraine RET"
    Then I should see "Ukraine"
