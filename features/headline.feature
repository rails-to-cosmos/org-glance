Feature: Headline
  Scenario: Create from org headline at point
    Given headline "sre"
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    Then the title of headline "sre" should be "Google SRE Book"
    And the contents of headline "sre" should be:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    And headline "sre" should be a book
    And headline "sre" should be an article

  Scenario: Create from the inside
    When I create an org file with contents:
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """
    And I goto the end of the buffer
    And I create headline from element at point
    Then headline title should be "Vocal lesson"
    And headline should be a task
    And headline contents should be:
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """

  Scenario: Serializable
    Given headline
      """
      *** Driving lesson :Education:
      - Key: Value
      """
    And I save headline to file "driving.org"
    And I load headline from file "driving.org"
    Then headline title should be "Driving lesson"
    And headline should be an education
    And headline should be propertized
    And headline contents should be:
      """
      * Driving lesson :Education:
      - Key: Value
      """

  Scenario: Normalized (indent level = 1)
    When I create an org file with contents:
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

  Scenario: No links in title allowed
    Given headline
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

  Scenario: Openable
    Given headline
      """
      * Release notes :Bookmark:
      - [[https://www.apple.com/workspace.htm][Release Notes]]
      """
    Then headline should contain links

  Scenario: Encryptable
    Given headline
      """
      * Release notes :Bookmark:
      aes-encrypted V 1.3-OCB-B-4-4-M
      ttT0N8RLii13eMJ+R25Z/o42dnw9M10+qZDOzH3PjMGzuVQYSxGuDZ2n8UUHwRvsQhgsizOGi4dE
      u65keAuJ/R1oskxb4dkIjFIW2ZLxoghkkb8j6xbLL9I=
      """
    Then headline should be encrypted

  Scenario: Propertizable
    Given headline
      """
      * Credentials
      Login: admin
      Password: admin
      """
    Then headline should be propertized

  Scenario: Archivable
    Given headline
      """
      * Old stuff :ARCHIVE:
      """
    Then headline should be archived

  Scenario: Commentable
    Given headline
      """
      * COMMENT Implicit stuff
      """
    Then headline should be commented

  Scenario: Closable
    Given headline
      """
      * DONE Hard work
      CLOSED: [2022-02-05 Sat 19:09]
      :LOGBOOK:
      - State "DONE"       from "TODO"       [2022-02-05 Sat 19:09]
      :END:
      """
    Then headline should be closed
