Feature: Headline
  @headline
  Scenario: Create from element at point
    When I create an org file with content:
      """
      * Google SRE Book :book:
      Managing Incidents
      """
    And I create headline from element at point
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
    And I create headline from element at point
    Then headline title should be "Google SRE Book"
    And headline should be a book
    And headline should be an article
    And headline contents should be:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """

  @headline @target
  Scenario: Headline should have normalized indentation
    When I create an org file with content:
      """
      *** Tags :article:
      Features and scenarios can be tagged using syntax @tag
      **** More indentations
      """
    And I create headline from element at point
    Then headline title should be "Tags"
    And headline should be an article
    And headline contents should be:
      """
      * Tags :article:
      Features and scenarios can be tagged using syntax @tag
      ** More indentations
      """

  @headline
  Scenario: Create from element with links in title, links should be cleared
    When I create an org file with content:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    And I create headline from element at point
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
    And I create headline from element at point
    Then headline title should be "Vocal lesson"
    And headline should be a task
    And headline contents should be:
      """
      * Vocal lesson :Task:
      - Mozart
      - Bach
      """

  @headline
  Scenario: Serde
    When I create an org file with content:
      """
      *** Driving lesson :Education:
      """
    And I create headline from element at point
    And I save headline to file "driving.org"
    And I load headline from file "driving.org"
    Then headline title should be "Driving lesson"
    And headline should be an education
    And headline contents should be:
      """
      * Driving lesson :Education:
      """

  @headline
  Scenario: Contains org-links
    When I create an org file with content:
      """
      * Release notes :Bookmark:
      - [[https://www.apple.com/workspace.htm][Release Notes]]
      """
    And I create headline from element at point
    Then headline should contain links
    And headline should not be encrypted

  @headline
  Scenario: Encrypted
    When I create an org file with content:
      """
      * Release notes :Bookmark:
      aes-encrypted V 1.3-OCB-B-4-4-M
      ttT0N8RLii13eMJ+R25Z/o42dnw9M10+qZDOzH3PjMGzuVQYSxGuDZ2n8UUHwRvsQhgsizOGi4dE
      u65keAuJ/R1oskxb4dkIjFIW2ZLxoghkkb8j6xbLL9I=
      """
    And I create headline from element at point
    Then headline should be encrypted
    And headline should not contain links
    And headline should not contain custom properties

  @headline
  Scenario: Contains custom properties
    When I create an org file with content:
      """
      * Credentials
      Login: admin
      Password: admin
      """
    And I create headline from element at point
    Then headline should contain custom properties

  @headline
  Scenario: Contains custom properties
    When I create an org file with content:
      """
      * Old stuff :ARCHIVE:
      """
    And I create headline from element at point
    Then headline should be archived

  @headline
  Scenario: Contains custom properties
    When I create an org file with content:
      """
      * COMMENT Implicit stuff
      """
    And I create headline from element at point
    Then headline should be commented

  @headline
  Scenario: Contains custom properties
    When I create an org file with content:
      """
      * DONE Hard work
      CLOSED: [2022-02-05 Sat 19:09]
      :LOGBOOK:
      - State "DONE"       from "TODO"       [2022-02-05 Sat 19:09]
      :END:
      """
    And I create headline from element at point
    Then headline should be closed
