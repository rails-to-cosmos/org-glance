@core
Feature: System Init
  In order to manage things
  As a user
  I want to create org-mode files and register org-glance views

  @org-mode
  Scenario: Create new org-mode file with tagged headlines
    Given I'm in a user directory
    And I create file "countries.org"
      """
      * Holland :Country:
      * Belgium :Country:
      * Georgia :Country:
      * TODO [#B] Ukraine :Country:
      """

  @scope
  Scenario: Register file in org-glance scope
    When I add file "countries.org" to default scope
    Then I should have 1 file in default scope
    And scope should contain file "countries.org"

  @view
  Scenario: Define org-glance view in default scope
    When I define view "Country" in default scope
    Then I should see message
      """
      View "Country" is now ready to glance
      """
    And I should have 1 view registered
    And I should have 4 headlines in view "Country"

  @view
  Scenario: Compile org-glance view to collect headlines and metadata
    Given I'm in a view directory
    And directory "country" does not exist
    Then I compile view "Country"
    And directory "country" should appear
    Then I change directory to "country"
    And I should see 2 files there
    And I should see file "country.el" with contents
      """
      `((nil nil nil))
      """
    And I should see file "country.org" with contents
      """
      * Holland :Country:
      :PROPERTIES:
      :some metadata should be here:
      :END:
      * Belgium :Country:
      * Georgia :Country:
      * TODO [#B] Ukraine :Country:
      """

  # @view @action
  # Scenario: Visit a headline
  #   When I run action "visit" for headlines and type "[Country] SPC Holland RET"
  #   Then I should see "Holland"

  # @view @action
  # Scenario: Visit a headline with priority
  #   When I run action "visit" for headlines and type "[Country] SPC Ukraine RET"
  #   Then I should see "Ukraine"
