(import spork/json)
(import http)
(import uri)

(def- version "57.0")
(var- session-id nil)
(var- url (get (os/environ) "SF_URL"))
(var- username (get (os/environ) "SF_USERNAME"))

(defn- escape [str]
  (->> str
       (string/replace-all "&" "&amp;")
       (string/replace-all "<" "&lt;")
       (string/replace-all ">" "&gt;")
       (string/replace-all `"` "&quote;")))

(defn- arrayish? [x] (in {:array true :tuple true} (type x)))

(defn- tableish? [x] (in {:table true :struct true} (type x)))

(var- tag nil) # this and varfn below enables mutual recursion

(defn- stringable?
  [x]
  (in {:nil true :boolean true :number true :string true
       :buffer true :symbol true :keyword true}
      (type x)))

(defn- stringafy
  [x]
  (if (nil? x) "null" (string x)))

(defn- to-xml
  "Convert given data structure to a simple XML string"
  [ds]
  (cond
    (arrayish? ds) (string/join (map |(to-xml $) ds))
    (tableish? ds) (string/join (map (fn [[name value]] (tag name value)) (pairs ds)))
    (error (string "Cannot stringify " (string ds)))))

(varfn tag [name value]
  (if (stringable? value)
    (string/format "<%s>%s</%s>" (string name) (stringafy value) (string name))
    (tag name (to-xml value))))


(defn- login-xml [username password]
  (string/format
   `<se:Envelope xmlns:se="http://schemas.xmlsoap.org/soap/envelope/">,
      <se:Header/>,
      <se:Body>,
        <login xmlns="urn:partner.soap.sforce.com">,
          <username>%s</username>,
          <password>%s</password>,
        </login>,
      </se:Body>,
    </se:Envelope>` (escape username) (escape password)))

(defn- tag-value [name xml]
  (let [open-tag (string "<" name ">")
        close-tag (string "</" name ">")
        tag-length (length open-tag)
        start (string/find open-tag xml)
        end (string/find close-tag xml)]
    (when start
      (string/slice xml (+ start tag-length) end))))

(defn- remove-html-entities
  [str]
  (->> str
       (string/replace-all "&apos;" "'" )))

(defn- parse-error
  "Retrieve the error message"
  [xml]
  {:success false
   :msg (remove-html-entities (tag-value "faultstring" xml))})

(defn- parse-error-message
  "Retrieve the error message"
  [xml]
  {:success false
   :msg (remove-html-entities (tag-value "message" xml))})

(defn- parse-session-id
  "Retrieve the session id"
  [xml]
  (let [url (tag-value "serverUrl" xml)
        session (tag-value "sessionId" xml)]
    session))

(defn- http-get
  "Get the page"
  [url]
  (http/get url :headers {"Authorization" (string "Bearer " session-id)}))

(defn- get-version
  "Doesn't require authorization. Current version is 57"
  []
  (if version version
      (let [path "/services/data/"
            res (http/get (string url path))
            cur (get (last (json/decode (res :body))) "version")]
        cur)))

(defn login
  "Login and retrieve current session token"
  []
  (let [env (os/environ)
        password (env "SF_PASSWORD")
        token (env "SF_TOKEN")
        _ (assert (and password token)
                  "SF_PASSWORD and SF_TOKEN must be set in environment")
        token+password (string password token)
        req (login-xml username token+password)
        loc (string url "/services/Soap/u/" version)
        resp (http/post loc req
                        :headers {"Content-Type" "text/xml" "SOAPAction" `""`})
        {:status status :body body} resp
        result (if (not (= status 200))
                 (parse-error body)
                 (parse-session-id body))]
    (if (= status 200)
      (do
        (set session-id result)
        session-id)
      (pp result))))

(defn describe
  `Get object metadata

   https://developer.salesforce.com/docs/atlas.en-us.244.0.api.meta/api/sforce_api_calls_describesobjects_describesobjectresult.htm`
  [name]
  (let [location (string url "/services/data/v" version "/sobjects/" name "/describe/")
        resp (http-get location)
        data (get resp :body)]
    (when (= (resp :status) 401)
      (login)
      (break (describe name)))
    (json/decode data)))

(defn picklist-values
  `Get picklist values for recordType

   https://developer.salesforce.com/docs/atlas.en-us.uiapi.meta/uiapi/ui_api_resources_picklist_values_collection.htm`
  [name field recordTypeId]
  (let [location (string url "/services/data/v" version "/ui-api/object-info/" name "/picklist-values/" recordTypeId)
        resp (http-get location)
        data (get resp :body)
        transformed (get-in (json/decode data) ["picklistFieldValues" field "values"])]
    (map | (put $ "active" true) transformed)
    (when (= (resp :status) 401)
      (login)
      (break (picklist-values name field recordTypeId))) transformed))

# todo This is very coarse
# These would be better
# https://github.com/plurals/pluralize/blob/master/pluralize.js
# https://alistapart.com/article/pluralization-for-javascript/
# https://www.grammarly.com/blog/plural-nouns/
(defn- pluralize [name]
  (string name "s"))

(defn- create-envelope [message]
  (string/format
   `<?xml version="1.0" encoding="UTF-8"?>
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                        xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <soapenv:Header xmlns="http://soap.sforce.com/2006/04/metadata">
          %s
        </soapenv:Header>
        <soapenv:Body xmlns="http://soap.sforce.com/2006/04/metadata">
          %s
        </soapenv:Body>
      </soapenv:Envelope>`
   (to-xml {:SessionHeader {:sessionId session-id}})
   message))

(defn- upsert-metadata-wrapper
  [type message]
  (string/format
   `<upsertMetadata>
     <metadata xsi:type="%s">
       %s
     </metadata>
   </upsertMetadata>` type (to-xml message)))

(defn- update-metadata-wrapper
  [type message]
  (string/format
   `<updateMetadata>
     <metadata xsi:type="%s">
       %s
     </metadata>
   </updateMetadata>` type (to-xml message)))

(defn- parse-success
  [xml]
  (let [created (tag-value "created" xml)
        fullName (tag-value "fullName" xml)
        success (tag-value "success" xml)]
    {:created (= created "true")
     :fullName fullName
     :success (= success "true")}))

(defn- metadata-upsert [kind metadata]
  (let [full-url (string url "/services/Soap/m/42.0")
        headers {"Authorization" (string "Bearer " session-id)
                 "Content-Type" "text/xml"
                 "SOAPAction" `""`}
        data (create-envelope (upsert-metadata-wrapper kind metadata))
        res (http/post full-url data :headers headers)
        body (res :body)
        success (= (tag-value "success" body) "true")]
    (cond
      (and (= success false) (= (res :status) 200)) (parse-error-message body)
      (= (res :status) 500) (parse-error body)
      (parse-success body))))

(defn metadata-upsert-object
  `Create or update object metadata. Fields are create seperately.`
  [{:name name :api_name api-name}]
  (let [metadata @{:fullName api-name
                   :label name
                   :pluralLabel (pluralize name)
                   # todo how to pick the name field?
                   :nameField {:type "AutoNumber" :label "Name"}
                   :deploymentStatus "Deployed"
                   :sharingModel "ReadWrite"}]
    (metadata-upsert "CustomObject" metadata)))

(defn metadata-upsert-field
  `Create or update field metadata.`
  [field-metadata]
  (metadata-upsert "CustomField" field-metadata))

(defn metadata-update-permission
  `Update admin profile.`
  [field]
  # if a field is required then it already has these permissions and
  # cannot be modified. Trying to do so will raise an exception.
  (when (field :required)
    (break {:created false
            :fullName (field :fullName)
            :success true}))
  (let [full-url (string url "/services/Soap/m/42.0")
        headers {"Authorization" (string "Bearer " session-id)
                 "Content-Type" "text/xml"
                 "SOAPAction" `""`}
        metadata {:fullName "Admin"
                  :fieldPermissions
                    {:field (field :fullName) :editable true :readable true}}
        data (create-envelope (update-metadata-wrapper "Profile" metadata))
        res (http/post full-url data :headers headers)
        body (res :body)
        success (= (tag-value "success" body) "true")]
    (cond
      (and (= success false) (= (res :status) 200)) (parse-error-message body)
      (= (res :status) 500) (parse-error body)
      (parse-success body))))

(defn- rest
  [method object data &opt id]
  (default id "")
  (let [url (string url "/services/data/v" version "/sobjects/" object "/" id)
        headers {"Authorization" (string "Bearer " session-id)
                 "Content-Type" "application/json"}
        thunk (case method
                :get http/get
                :put http/put
                :post http/post
                :patch http/patch
                :delete http/delete)
        body (when data (json/encode data))
        resp (if (= method :delete)
               (thunk url :headers headers)
               (thunk url body :headers headers))
        resp-body (resp :body)
        result (json/decode (if (= resp-body "") "{}" resp-body))]
    (cond
      (= (resp :status) 401)
      (do
        (login)
        (rest method object data id))
      (get-in result [0 "errorCode"])
      {:success false
       :msg (get-in result [0 "message"])
       :errorCode (get-in result [0 "errorCode"])}
      (= (resp :status) 500)
      {:success false
       :msg (resp :message)
       :errorCode (resp :errorCode)}
      {:success true
       :result result})))

(defn create
  "Create an object"
  [object data]
  (rest :post object data))

(defn update
  "Update an object"
  [object id data]
  (rest :patch object data id))

(defn delete
  "Delete an object"
  [object id]
  (rest :delete object nil id))

(defn query
  "Run SOQL"
  [soql]
  (let [escaped-soql (uri/escape soql)
        url (string url "/services/data/v" version "/query/?q=" escaped-soql)
        headers {"Authorization" (string "Bearer " session-id)}
        resp (http/get url :headers headers)
        resp-body (resp :body)
        result (json/decode (if (= resp-body "") "{}" resp-body))]
    (case (resp :status)
      401 (do (login) (query soql))
      500 {:success false
           :msg (resp :message)
           :errorCode (resp :errorCode)}
      {:success true
       :result result})))

(defn get-blob
  "Retrieve Blob Data stored in sObject field

   https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/dome_sobject_blob_retrieve.htm"
  [sobject id blob-field]
  (let [url (string url "/services/data/v" version "/sobjects/" sobject "/" id "/" blob-field)
        headers {"Authorization" (string "Bearer " session-id)}
        resp (http/get url :headers headers)]
    (case (resp :status)
      401 (do (login) (get-blob sobject id blob-field))
      500 {:success false
           :msg (resp :message)
           :errorCode (resp :errorCode)}
      {:success true
       :result (resp :body)})))
