package queue

import (
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/IBM/sarama"
)

type KafkaClient struct {
	producer sarama.SyncProducer
}

func newConnectionKafka() sarama.SyncProducer {
	config := sarama.NewConfig()
	config.Producer.Return.Successes = true

	client, err := sarama.NewClient(strings.Split(settingsData.KAFKA_HOSTS, ","), config)
	if err != nil {
		panic(err)
	}
	producer, err := sarama.NewSyncProducerFromClient(client)
	if err != nil {
		panic(err)
	}

	return producer
}

func (k KafkaClient) Publish(event bus.Event) error {
	msg := &sarama.ProducerMessage{
		Topic: string(event.Name),
		Value: sarama.ByteEncoder(event.Payload),
	}

	_, _, err := k.producer.SendMessage(msg)
	return err
}

func (k KafkaClient) Request(name bus.Event, toBind interface{}) error {
	panic("unimplemented")
}

func (k KafkaClient) Subscribe(name bus.EventName, handler func(c bus.Context) error) {
	panic("unimplemented")
}

func (k KafkaClient) SubscribeAndRespond(name bus.EventName, handler func(c bus.Context) (*bus.BusResponse, error)) {
	panic("unimplemented")
}

func NewKafka() bus.Bus {
	producer := newConnectionKafka()

	return KafkaClient{
		producer: producer,
	}
}
