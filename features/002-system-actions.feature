# @core
# Feature: Compilation
#   In order to manage views
#   As a user
#   I want to define views

#   @core
#   Scenario: Directory structure
#     Given I'm in a root directory

#     # Implement directory structure for user data and system data
#     And I create directory "org-glance-views"
#     And I create directory "user-data"

#     # Set org-glance-view-location
#     When I change directory to "org-glance-views"
#     And I set view location to current directory
#     And I change directory to parent directory

#   @scope
#   Scenario: Using the default scope: list of files
#     When I change directory to "user-data"
#     And I create org-mode file "countries.org"
#       """
#       * Holland :Country:
#       * Belgium :Country:
#       * Georgia :Country:
#       * TODO [#B] Ukraine :Country:
#       """
#     When I add the file to default scope
#     Then I should have 1 file in default scope
#     And I change directory to parent directory

#   @scope @view
#   Scenario: Define view
#     When I define view "Country" with default scope
#     Then I should see message
#       """
#       View "Country" is now ready to glance
#       """
#     And I should have 4 headlines in view "Country"
#     And I should have 1 view registered

#   @scope @view
#   Scenario: Compile view
#     When I update view "Country"
