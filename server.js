const express = require('express')
const bodyParser = require('body-parser')
const crypto = require('crypto')
const app = express()
const port = 3001
const fs = require('fs')

app.use(bodyParser.json());
app.use('/storage', express.static('./storage'))

// Test with:
// curl -d '{"key1":"value1", "key2":"value2"}' -H "Content-Type: application/json" -X POST http://localhost:3001/store

app.post('/store',
    (req, res) => {
        console.log(req.body);
        const current_date = (new Date()).valueOf().toString()
        const random = Math.random().toString()
        const hash = crypto.createHash('sha1').update(current_date + random).digest('hex')
        // FIXME: add version
        fs.writeFile("./storage/" + hash + ".json", JSON.stringify(req.body), function(err) {

            if(err) {
                return console.log(err);
            }

            console.log("The file was saved!");
        })
        res.send(hash)
    }
);

app.listen(port, () => console.log(`Example app listening on port ${port}!`))
