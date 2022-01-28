Feature: Headline
  @headline
  Scenario: Create from element at point
    When I create an org file with content:
      """
      * Google SRE Book :book:
      Managing Incidents
      """
    Then headline title should be "Google SRE Book"
    And headline should be a book
    And headline contents should be:
      """
      * Google SRE Book :book:
      Managing Incidents
      """

  @headline
  Scenario: Create from element at point
    When I create an org file with content:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    Then headline title should be "Google SRE Book"
    And headline should be a book
    And headline should be an article
    And headline contents should be:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """

  @headline
  Scenario: Create from indented element at point
    When I create an org file with content:
      """
      *** Tags :article:
      Features and scenarios can be tagged using syntax @tag
      """
    And headline title should be "Tags"
    And headline should be an article
    And headline contents should be:
      """
      * Tags :article:
      Features and scenarios can be tagged using syntax @tag
      """

  @headline
  Scenario: Create from element with links in title, links should be cleared
    When I create an org file with content:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    Then headline title should be "Transmission bookmark: Transmission Web UI"
    And headline should be a bookmark
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
    And headline should be a task
    And headline contents should be:
      """
      * Vocal lesson :Task:
      - Mozart
      - Bach
      """
