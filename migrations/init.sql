CREATE TABLE opQueue
(
    queueId UUID NOT NULL PRIMARY KEY,
    createdAt timestamp with time zone NOT NULL,
    processingStartedAt timestamp with time zone,
    errors TEXT,
    op json
);

CREATE INDEX ix_opQueue on
    opQueue (processingStartedAt, createdAt ASC)
INCLUDE (queueId)
WHERE processingStartedAt IS NULL;

CREATE TABLE documents
(
    documentId serial PRIMARY KEY,
    payload TEXT
);