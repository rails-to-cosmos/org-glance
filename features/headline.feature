Feature: Headline
  Scenario: Create from headline at point (point at title)
    Given buffer "*books*"
    """
    * Google SRE Book :book:article:
    Managing Incidents
    """

    Then the title of headline "Google SRE Book" should be "Google SRE Book"
    And the contents of headline "Google SRE Book" should be
      """
      Managing Incidents
      """
    And headline "Google SRE Book" should be a book
    And headline "Google SRE Book" should be an article

  Scenario: Create from headline at point (point inside subtree)
    Given buffer "*tasks*"
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """
    And I goto the end of the buffer
    Then the title of headline at point should be "Vocal lesson"
    And headline at point should be a task
    And headline at point contents should be
      """
      - Mozart
      - Bach
      """

  # Scenario: Serializable contents
  #   Given headline "lesson"
  #     """
  #     *** Driving lesson :Education:
  #     - Key: Value
  #     """
  #   And I save headline "lesson" to file "driving.org"
  #   And I load headline "driving" from file "driving.org"
  #   Then the title of headline "driving" should be "Driving lesson"
  #   And headline "driving" should be equal to headline "lesson"

  # Scenario: Normalized indentation
  #   Given buffer "*indented*"
  #     """
  #     *** Tags :article:
  #     Features and scenarios can be tagged using syntax @tag
  #     **** More indentations
  #     """
  #   Given buffer "*unindented*"
  #     """
  #     * Tags :article:
  #     Features and scenarios can be tagged using syntax @tag
  #     ** More indentations
  #     """
  #   Then headline "indented" should be equal to headline "unindented"

  Scenario: No links in title
    Given buffer "*bookmarks*"
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    Then the title of headline "Transmission bookmark: Transmission Web UI" should be "Transmission bookmark: Transmission Web UI"
    And headline "Transmission bookmark: Transmission Web UI" should be a bookmark
    And headline "Transmission bookmark: Transmission Web UI" should contain link
    And the contents of headline "Transmission bookmark: Transmission Web UI" should be
      """
      - Uses fewer resources than other clients
      """

  Scenario: Headline contains links
    Given buffer "*bookmarks*"
      """
      * Release notes :Bookmark:
      - [[https://www.apple.com/workspace.htm][Release Notes]]
      """
    Then headline "Release notes" should contain links

  Scenario: Headline is encrypted
    Given buffer "*bookmarks*"
      """
      * Release notes :Bookmark:
      aes-encrypted V 1.3-OCB-B-4-4-M
      ttT0N8RLii13eMJ+R25Z/o42dnw9M10+qZDOzH3PjMGzuVQYSxGuDZ2n8UUHwRvsQhgsizOGi4dE
      u65keAuJ/R1oskxb4dkIjFIW2ZLxoghkkb8j6xbLL9I=
      """
    Then headline "Release notes" should be encrypted

  Scenario: Headline contains properties specified by user
    Given buffer "*credentials*"
      """
      * Credentials
      Login: admin
      Password: admin
      """
    Then headline "Credentials" should be propertized

  Scenario: Archived headline
    Given buffer "*archive*"
      """
      * Old stuff :ARCHIVE:
      """
    Then headline "Old stuff" should be archived

  Scenario: Commented headline
    Given buffer "*comments*"
      """
      * COMMENT Implicit stuff
      """
    Then headline "Implicit stuff" should be commented

  Scenario: Closed headline
    Given buffer "*done*"
      """
      * DONE Hard work
      CLOSED: [2022-02-05 Sat 19:09]
      :LOGBOOK:
      - State "DONE"       from "TODO"       [2022-02-05 Sat 19:09]
      :END:
      """
    Then headline "Hard work" should be closed
