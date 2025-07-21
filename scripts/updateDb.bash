cd ./db && npx prisma db push
cd ../
rm -rf src/package/db/models/
sqlboiler psql
