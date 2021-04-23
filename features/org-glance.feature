Feature: Do Some things
  In order to perform org-glance actions on views
  As a user
  I want to define org-glance views

  Scenario: Run org-glance on specific org-mode file
    Given empty scope
    And file with contents
      """
      * Netherlands :Country:
      * Belgium     :Country:
      * Georgia     :Country:
      * Ukraine     :Country:
      """
    Then I add the file to scope
    Then I should have 1 file in scope
