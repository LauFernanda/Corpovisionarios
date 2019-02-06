import scrapy


class StackOverflowSpider(scrapy.Spider):
    name = 'espectador CC'
    start_urls = ['http://www.elespectador.com/tags/cultura-ciudadana']

    def parse(self, response):
        for href in response.css('.caption h3 a::attr(href)'):
            full_url = response.urljoin(href.extract())
            yield scrapy.Request(full_url, callback=self.parse_question)

    def parse_question(self, response):
        yield {
            'titulo': response.css('.titulo h1 a::text').extract()[0], 
            'lead': response.css('.lead').extract()[0],
            'body': response.css('.nota .content_nota').extract()[0],
            'tags': response.css('.nota .tag-no-vacio::text').extract(),
            'fecha':  response.css('.noticia_apertura .hora'.extract()[0],
            'link': response.url,
        }
