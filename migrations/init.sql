CREATE TABLE op_queue
(
    queue_id UUID NOT NULL PRIMARY KEY,
    created_at timestamp with time zone NOT NULL,
    processing_started_at timestamp with time zone,
    errors TEXT,
    pending_changes json
);

CREATE INDEX ix_op_queue on
    op_queue (processing_started_at, created_at ASC)
INCLUDE (queue_id)
WHERE processing_started_at IS NULL;

CREATE TABLE documents
(
    document_id serial PRIMARY KEY,
    payload TEXT,
    revision_log json
);