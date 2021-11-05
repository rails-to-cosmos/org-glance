Feature: Headline

  @headline
  Scenario: Create from element at point
    When I create an org file with content:
"""
* Google SRE Book :book:mgmt:
Managing Incidents
"""
    Then headline title should be "Google SRE Book"
    And headline classes should be "book, mgmt"
    And headline contents should be:
"""
* Google SRE Book :book:mgmt:
Managing Incidents
"""

  @headline
  Scenario: Create from indented element at point
    When I create an org file with content:
      """
      *** Tags :doc:
      Features and scenarios can be tagged using syntax @tag
      """
    Then headline title should be "Tags"
    And headline classes should be "doc"
    And headline contents should be:
      """
      * Tags :doc:
      Features and scenarios can be tagged using syntax @tag
      """

  @headline
  Scenario: Create from element with links in title
    When I create an org file with content:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    Then headline title should be "Transmission bookmark: Transmission Web UI"
    And headline classes should be "bookmark"
    And headline contents should be:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """

  @headline
  Scenario: Create from the inside
    When I create an org file with content:
      """
      * Vocal lesson :Task:
      - Mozart
      - Bach
      """
    And I goto the end of the buffer
    Then headline title should be "Vocal lesson"
    And headline classes should be "task"
    And headline contents should be:
      """
      * Vocal lesson :Task:
      - Mozart
      - Bach
      """
