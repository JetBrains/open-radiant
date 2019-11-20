const express = require('express')
const bodyParser = require('body-parser')
const cors = require('cors')
const crypto = require('crypto')
const app = express()
const port = 3001
const fs = require('fs')

app.use(cors())
app.use(bodyParser.json());
app.use('/storage', express.static('./storage'))

// Test with:
// curl -d '{"key1":"value1", "key2":"value2"}' -H "Content-Type: application/json" -X POST http://localhost:3001/store

const forSave = hash => hash.substring(0, 8)
const forUser = hash => hash.substring(0, 4) + '-' + hash.substring(4, 8);
const fromUser = userHash => userHash.substring(0, 4) + userHash.substring(5, 9)

app.post('/store',
    (req, res) => {
        console.log(req.body);
        const currentDate = (new Date()).valueOf().toString()
        const random = Math.random().toString()
        const hash = crypto.createHash('sha1').update(currentDate + random).digest('hex')
        req.body.savedAt = currentDate
        const filePath = "./storage/" + forSave(hash) + ".json";
        fs.writeFile(filePath, JSON.stringify(req.body), maybeErr => {

            if (maybeErr) {
                console.error(maybeErr)
                res.status(400).end()
                return;
            }

            console.log('Received the scene and stored it as ' + filePath)
        });
        res.json({ 'hash': forUser(hash) })
    }
);

app.post('/load',
    (req, res) => {
        const hash = req.body['hash']
        console.log(req.body)
        console.log(hash)

        if (!hash) {
            console.error('No hash found in request body')
            res.status(400).end()
            return;
        }

        const filePath = './storage/' + fromUser(hash) + '.json'
        fs.readFile(filePath, (maybeErr, jsonBuffer) => {

            if (maybeErr) {
                console.error(maybeErr)
                res.status(400).end()
                return;
            }

            console.log('Responding to the request with the scene from ' + filePath)

            res.send(jsonBuffer)

        })
    }
);

// TODO: `.get('/load')`

app.listen(port, () => console.log(`Server is listening on port ${port}!`))
