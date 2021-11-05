Feature: ClassRegistry

  @classregistry
  Scenario: Create a new class
    When I define class "author"
    Then I should have 1 active class in class registry

  @classregistry
  Scenario: Read class registry
    When I create directory "article" in org glance directory
    And I update class registry
    Then I should have 1 active class in class registry
