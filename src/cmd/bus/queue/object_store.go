package queue

import (
	"io"

	"github.com/nats-io/nats.go"
)

// ObjectStores
const (
	FilesOS = "files"
)

type objectStoreNATS struct {
	os nats.ObjectStore
}

var objectStore = map[string]*objectStoreNATS{}

// Funcs
func (osNATS *objectStoreNATS) Put(
	obj *nats.ObjectMeta,
	reader io.Reader,
	opts ...nats.ObjectOpt,
) (*nats.ObjectInfo, error) {
	return osNATS.os.Put(obj, reader, opts...)
}

func (osNATS *objectStoreNATS) Delete(name string) error {
	return osNATS.os.Delete(name)
}

func (osNATS *objectStoreNATS) Get(name string) ([]byte, error) {
	return osNATS.os.GetBytes(name)
}

// New ObjectStore
func (natsClient *NatsClient) ObjectStore(bucketName string) *objectStoreNATS {
	var exists bool
	var bucket *objectStoreNATS

	if bucket, exists = objectStore[bucketName]; !exists {
		os, err := natsClient.jsContext.CreateObjectStore(&nats.ObjectStoreConfig{
			Bucket:   bucketName,
			MaxBytes: 2.5e+7,
			Storage:  nats.FileStorage,
		})
		if err != nil {
			panic(err)
		}
		bucket = &objectStoreNATS{
			os: os,
		}
		objectStore[bucketName] = bucket
	}
	return bucket
}
