Feature: Metastore
  @metastore
  Scenario: Create empty metastore
    When I define class "Author"
    Then I should have 1 active class
