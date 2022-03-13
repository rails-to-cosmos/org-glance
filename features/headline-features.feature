Feature: Headline features
  Scenario: Openable
    When I create an org file with content:
      """
      * Release notes :Bookmark:
      - [[https://www.apple.com/workspace.htm][Release Notes]]
      """
    And I create headline from element at point
    Then headline should contain links

  Scenario: Encryptable
    When I create an org file with content:
      """
      * Release notes :Bookmark:
      aes-encrypted V 1.3-OCB-B-4-4-M
      ttT0N8RLii13eMJ+R25Z/o42dnw9M10+qZDOzH3PjMGzuVQYSxGuDZ2n8UUHwRvsQhgsizOGi4dE
      u65keAuJ/R1oskxb4dkIjFIW2ZLxoghkkb8j6xbLL9I=
      """
    And I create headline from element at point
    Then headline should be encrypted

  Scenario: Propertizable
    When I create an org file with content:
      """
      * Credentials
      Login: admin
      Password: admin
      """
    And I create headline from element at point
    Then headline should be propertized

  Scenario: Archivable
    When I create an org file with content:
      """
      * Old stuff :ARCHIVE:
      """
    And I create headline from element at point
    Then headline should be archived

  @headline
  Scenario: Commentable
    When I create an org file with content:
      """
      * COMMENT Implicit stuff
      """
    And I create headline from element at point
    Then headline should be commented

  @headline
  Scenario: Closable
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
